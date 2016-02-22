{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Mafia.Hash
  ( HashError(..)
  , renderHashError

  , Hash(..)
  , renderHash
  , parseHash

  , hashHashes
  , hashBytes
  , hashText
  , hashFile
  , tryHashFile
  ) where

import qualified Crypto.Hash.SHA1 as SHA1

import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Mafia.IO
import           Mafia.Path

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left)

------------------------------------------------------------------------

data HashError =
    HashFileNotFound File
    deriving (Show)

renderHashError :: HashError -> Text
renderHashError = \case
  HashFileNotFound file ->
    "File not found: " <> file

------------------------------------------------------------------------

newtype Hash =
  Hash {
      unHash :: ByteString
    } deriving (Eq, Ord, Show)

renderHash :: Hash -> Text
renderHash =
  T.decodeUtf8 . Base16.encode . unHash

parseHash :: Text -> Maybe Hash
parseHash hex =
  let (bs, _) = Base16.decode (T.encodeUtf8 hex)
  in if B.length bs == 20
     then Just (Hash bs)
     else Nothing

------------------------------------------------------------------------

hashHashes :: [Hash] -> Hash
hashHashes =
  Hash . SHA1.hashlazy . L.fromChunks . fmap unHash

hashFile :: File -> EitherT HashError IO Hash
hashFile file = do
  mbytes <- readBytes file
  case mbytes of
    Nothing    -> left (HashFileNotFound file)
    Just bytes -> pure (hashBytes bytes)

tryHashFile :: File -> EitherT HashError IO (Maybe Hash)
tryHashFile file = do
  mbytes <- readBytes file
  pure $ fmap hashBytes mbytes

hashText :: Text -> Hash
hashText text = do
  hashBytes (T.encodeUtf8 text)

hashBytes :: ByteString -> Hash
hashBytes bytes = do
  Hash (SHA1.hash bytes)

------------------------------------------------------------------------

instance ToJSON Hash where
  toJSON =
    String . renderHash

instance FromJSON Hash where
  parseJSON = \case
    String txt ->
      case parseHash txt of
        Nothing  -> fail ("invalid hash: " <> T.unpack txt)
        Just pid -> pure pid
    _ ->
      mzero
