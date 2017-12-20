{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Twine.Async (
    AsyncTimeout (..)
  , renderAsyncTimeout
  , waitWithTimeout
  , waitEitherBoth
  ) where

import           Control.Concurrent.Async (Async, waitSTM, waitEither)
import           Control.Concurrent.Async (async, cancel, wait)
import           Control.Concurrent.STM (atomically, orElse, retry)
import           Control.Exception.Base (AsyncException (..))
import           Control.Monad.Catch (catch, throwM)

import           Control.Monad.IO.Class (liftIO)

import           Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T

import           P

import           Mafia.Twine.Snooze

import           System.IO (IO)

import           X.Control.Monad.Trans.Either

data AsyncTimeout =
  AsyncTimeout Duration
  deriving (Eq, Show)

renderAsyncTimeout :: AsyncTimeout -> Text
renderAsyncTimeout e =
  case e of
    AsyncTimeout d ->
      mconcat [
          "Async took greater than '"
        , T.pack . show $ toSeconds d
        , " seconds' to return."
        ]

waitWithTimeout :: Async a -> Duration -> EitherT AsyncTimeout IO a
waitWithTimeout a d = do
  r <- liftIO $ newIORef False
  s <- liftIO . async $ snooze d
  e <- liftIO $ waitEither a s
  case e of
    Left a' ->
      pure $ a'
    Right _ -> do
      liftIO $ writeIORef r True
      liftIO $ cancel a
      (liftIO $ wait a)
        `catch`
          (\(ae :: AsyncException) ->
            case ae of
              ThreadKilled -> do
                liftIO (readIORef r) >>=
                  bool (liftIO $ throwM ae) (left $ AsyncTimeout d)
              StackOverflow ->
                liftIO $ throwM ae
              HeapOverflow ->
                liftIO $ throwM ae
              UserInterrupt ->
                liftIO $ throwM ae)

waitEitherBoth :: Async a -> Async b -> Async c -> IO (Either a (b, c))
waitEitherBoth a b c =
  atomically $ do
    let
      l = waitSTM a
      r = do
        bb <- waitSTM b `orElse` (waitSTM c >> retry)
        cc <- waitSTM c
        return (bb, cc)
    fmap Left l `orElse` fmap Right r
