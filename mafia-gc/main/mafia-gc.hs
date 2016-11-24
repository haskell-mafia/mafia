{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where


import           Control.Monad.IO.Class  (MonadIO(..))
import           Control.Monad.Trans.Resource  (MonadResource(..), runResourceT)

import           Data.Conduit (Consumer, Producer, ($$), (=$=))
import qualified Data.Conduit.Find as F
import qualified Data.Conduit.List as CL
import           Data.Set  (Set)
import qualified Data.Set as S

import           P

import           System.Directory (removeDirectoryRecursive)
import           System.Environment  (getArgs)
import           System.FilePath.Posix  ((</>), takeDirectory, takeFileName)
import           System.IO  (FilePath, IO, putStrLn)
import qualified System.Posix.Files as U


data GcRun
  = ListUsed
  | ListUnused
  | RmRfUnused


main :: IO ()
main = do
  args <- getArgs
  case args of
    (x:y:_) ->
      -- TODO canonicalise dirs
      run ListUnused x y
    _ ->
      putStrLn "Usage: mafia-gc MAFIA_DIR DIR"

run :: GcRun -> FilePath -> FilePath -> IO ()
run runt mafia dir = do
  let cache = mafia </> "packages"
      bins = mafia </> "bin"
  used <- runResourceT (findLinkedPackages cache dir $$ foldSet)
  binUsed <- runResourceT (findLinkedBinaries cache bins $$ foldSet)
  let allUsed = S.union used binUsed
      getUnused sink = runResourceT $
            findAllPackages cache
        =$= CL.filter (\f -> not (S.member f allUsed))
        =$= CL.filter (\f -> not (S.member (takeFileName f) specialFiles))
         $$ sink
  case runt of
    ListUsed ->
      for_ (S.toList allUsed) putStrLn

    ListUnused ->
      getUnused (CL.mapM_ (liftIO . putStrLn))

    RmRfUnused -> do
      ded <- getUnused CL.consume
      for_ ded removePackage
      putStrLn $ "Deleted " <> show (length ded) <> " packages."

specialFiles :: Set FilePath
specialFiles = S.fromList [
    ".locks"
  ]

removePackage :: FilePath -> IO ()
removePackage fp =
  -- TODO need to clean up the locks here
  removeDirectoryRecursive fp

foldSet :: Monad m => Consumer (a, FilePath) m (Set FilePath)
foldSet =
  CL.fold (flip (S.insert . snd)) mempty

findLinkedPackages ::
     (MonadIO m, MonadResource m)
  => FilePath -> FilePath -> Producer m (F.FileEntry, FilePath)
findLinkedPackages cache dir =
  F.sourceFindFiles findOpts dir $ do
    F.glob "*.conf"
    conf <- isInCache cache
    pure (takeDirectory conf)

findLinkedBinaries ::
     (MonadIO m, MonadResource m)
  => FilePath -> FilePath -> Producer m (F.FileEntry, FilePath)
findLinkedBinaries cache bin =
  F.sourceFindFiles findOpts bin $ do
    d <- F.getDepth
    guard (d == 1)
    isInCache cache

findAllPackages ::
     (MonadIO m, MonadResource m)
  => FilePath -> Producer m FilePath
findAllPackages cache =
  F.findFilePaths findOpts cache $ do
    d <- F.getDepth
    guard (d == 3)
    F.norecurse

isInCache :: MonadIO m => FilePath -> F.CondT F.FileEntry m FilePath
isInCache cache = do
  F.hasStatus U.isSymbolicLink
  n <- F.getFilePath
  p <- liftIO (U.readSymbolicLink n)
  guard (isPrefixOf cache p)
  F.norecurse
  pure p

findOpts :: F.FindOptions
findOpts =
  F.defaultFindOptions
  {F.findFollowSymlinks = False, F.findContentsFirst = False}
