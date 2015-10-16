{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Mafia.Chaos where

import           Control.Exception (IOException)
import           Control.Monad.Catch (MonadCatch(..), bracket, handle)
import           Control.Monad.IO.Class (MonadIO(..))

import           Disorder.Corpus (muppets, viruses)
import           Disorder.Core.IO (testIO)

import           Data.Char (toUpper)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mafia.IO
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO, print)
import           System.IO.Temp (withSystemTempDirectory)

import           Test.QuickCheck (Arbitrary(..), Gen, Property, Testable(..))
import           Test.QuickCheck (forAllProperties)
import qualified Test.QuickCheck as QC

import           X.Control.Monad.Trans.Either (EitherT(..))

------------------------------------------------------------------------

type PackageName   = Text
type SubmoduleName = Text

data Repo = Repo {
    repoFocusPackage  :: PackageDeps
  , repoOtherPackages :: Map PackageName PackageDeps
  } deriving (Eq, Ord, Show)

data PackageDeps = PackageDeps {
    depSubmodules    :: Set SubmoduleName
  , depLocalPackages :: Set PackageName
  } deriving (Eq, Ord, Show)

newtype Actions = Actions { unActions :: [Action] }
  deriving (Eq, Ord, Show)

data Action =
    Build
  | AddLocal     PackageName   PackageName
  | AddSubmodule SubmoduleName PackageName
  | RemoveLocal     PackageName
  | RemoveSubmodule SubmoduleName
  -- Sidestep = temporarily move out the way, try to build, then move back to
  -- the valid location. Useful to test that caching works even across invalid
  -- source states.
  | SidestepLocal     PackageName
  | SidestepSubmodule SubmoduleName
  deriving (Eq, Ord, Show)

repoSubmodules :: Repo -> Set SubmoduleName
repoSubmodules (Repo focus others) =
  let fs = depSubmodules focus
      os = Set.unions (fmap depSubmodules (Map.elems others)) in
  Set.union fs os

emptyRepo :: Repo
emptyRepo = Repo emptyPackageDeps Map.empty

emptyPackageDeps :: PackageDeps
emptyPackageDeps = PackageDeps Set.empty Set.empty

fromTestRun :: [(Action, Repo)] -> [Action]
fromTestRun = fmap fst

fromActions :: [Action] -> Maybe [(Action, Repo)]
fromActions = fromActions' emptyRepo

fromActions' :: Repo -> [Action] -> Maybe [(Action, Repo)]
fromActions' _    []       = Just []
fromActions' repo (a : as) = do
  repo' <- applyAction a repo
  xs    <- fromActions' repo' as
  pure $ (a, repo') : xs

finalRepo :: [(Action, Repo)] -> Repo
finalRepo = \case
  []             -> emptyRepo
  ((_, repo):[]) -> repo
  (_:xs)         -> finalRepo xs

------------------------------------------------------------------------

instance Arbitrary Actions where
  arbitrary =
    fmap (Actions . fromTestRun) . QC.sized $ \n -> do
      k <- QC.choose (0, n)
      genTestRun k

  shrink (Actions as) = Actions <$> do
    as' <- QC.shrinkList shrinkAction as
    case fromActions as' of
      Nothing -> pure []
      Just _  -> pure as'

shrinkAction :: Action -> [Action]
shrinkAction = \case
  AddLocal     dep pkg
   | pkg /= focusPackageName -> [AddLocal     dep focusPackageName]
   | otherwise               -> []
  AddSubmodule dep pkg
   | pkg /= focusPackageName -> [AddSubmodule dep focusPackageName]
   | otherwise               -> []
  Build                      -> []
  RemoveLocal     _          -> []
  RemoveSubmodule _          -> []
  SidestepLocal     _        -> []
  SidestepSubmodule _        -> []

genTestRun :: Int -> Gen [(Action, Repo)]
genTestRun n
  | n <= 0    = pure []
  | otherwise = do
      testRun <- genTestRun (n - 1)

      let repo = finalRepo testRun
      action <- genAction (finalRepo testRun)

      pure . fromMaybe testRun
           . fmap (\r -> testRun <> [(action, r)])
           $ applyAction action repo

genAction :: Repo -> Gen Action
genAction repo@(Repo _ others) = do
  mbLocalPackageNames <- oneOfSet localPackageNames
  mbSubmoduleNames    <- oneOfSet submoduleNames

  let removableLocals = Map.keysSet others
      availableLocals = Set.insert focusPackageName
                      . maybe id Set.delete mbLocalPackageNames
                      $ removableLocals

  mbRemovableLocals     <- oneOfSet removableLocals
  mbAvailableLocals     <- oneOfSet availableLocals
  mbRemovableSubmodules <- oneOfSet (repoSubmodules repo)

  QC.elements $ catMaybes [
      Just Build
    , AddLocal          <$> mbLocalPackageNames <*> mbAvailableLocals
    , AddSubmodule      <$> mbSubmoduleNames    <*> mbAvailableLocals
    , RemoveLocal       <$> mbRemovableLocals
    , RemoveSubmodule   <$> mbRemovableSubmodules
    , SidestepLocal     <$> mbRemovableLocals
    , SidestepSubmodule <$> mbRemovableSubmodules
    ]

focusPackageName :: PackageName
focusPackageName = "chaos-focus"

localPackageNames :: Set PackageName
localPackageNames = Set.fromList (("chaos-" <>) . T.replace " " "-" <$> viruses)

submoduleNames :: Set SubmoduleName
submoduleNames = Set.fromList (T.replace " " "-" <$> muppets)

oneOfSet :: Set a -> Gen (Maybe a)
oneOfSet xs
  | Set.null xs = pure Nothing
  | otherwise   = Just <$> QC.elements (Set.toList xs)

applyAction :: Action -> Repo -> Maybe Repo
applyAction action =
  let mustModify f g repo = if g repo == repo
                            then Nothing
                            else Just (f (repo, g repo))
  in case action of
    Build                     -> Just . id
    AddLocal          dep pkg -> mustModify snd (addLocal        dep pkg)
    AddSubmodule      dep pkg -> mustModify snd (addSubmodule    dep pkg)
    RemoveLocal       dep     -> mustModify snd (removeLocal     dep)
    RemoveSubmodule   dep     -> mustModify snd (removeSubmodule dep)
    SidestepLocal     dep     -> mustModify fst (removeLocal     dep)
    SidestepSubmodule dep     -> mustModify fst (removeSubmodule dep)

addLocal :: PackageName -> PackageName -> Repo -> Repo
addLocal dep pkg repo@(Repo f os)
  | pkgIsFocus = Repo (addLocal' dep f) os'
  | isCircular = repo
  | pkgExists  = Repo f (Map.adjust (addLocal' dep) pkg os')
  | otherwise  = repo
  where
    pkgIsFocus = pkg == focusPackageName
    isCircular = Set.member pkg depDeps
    pkgExists  = Map.member pkg os

    depDeps = fromMaybe Set.empty (recursiveLocals repo <$> Map.lookup dep os')
    os'     = Map.insertWith (\_ old -> old) dep emptyPackageDeps os

addLocal' :: PackageName -> PackageDeps -> PackageDeps
addLocal' dep (PackageDeps ss ls) =
  PackageDeps ss (Set.insert dep ls)

recursiveLocals :: Repo -> PackageDeps -> Set PackageName
recursiveLocals repo@(Repo _ os) (PackageDeps _ ls) =
  let lookup name = fromMaybe Set.empty (recursiveLocals repo <$> Map.lookup name os) in
  Set.union (unionMap lookup ls) ls

unionMap :: Ord b => (a -> Set b) -> Set a -> Set b
unionMap f = Set.foldl (\xs x -> xs `Set.union` f x) Set.empty

addSubmodule :: SubmoduleName -> PackageName -> Repo -> Repo
addSubmodule dep pkg (Repo f os) =
  if pkg == focusPackageName
  then Repo (addSubmodule' dep f) os
  else Repo f (Map.adjust (addSubmodule' dep) pkg os)

addSubmodule' :: SubmoduleName -> PackageDeps -> PackageDeps
addSubmodule' dep (PackageDeps ss ls) =
  PackageDeps (Set.insert dep ss) ls

removeLocal :: PackageName -> Repo -> Repo
removeLocal dep (Repo f os) =
  let rm = Map.map (removeLocal' dep) . Map.delete dep in
  Repo (removeLocal' dep f) (rm os)

removeLocal' :: PackageName -> PackageDeps -> PackageDeps
removeLocal' dep (PackageDeps ss ls) =
  PackageDeps ss (Set.delete dep ls)

removeSubmodule :: SubmoduleName -> Repo -> Repo
removeSubmodule dep (Repo f os) =
  let rm = Map.map (removeSubmodule' dep) . Map.delete dep in
  Repo (removeSubmodule' dep f) (rm os)

removeSubmodule' :: SubmoduleName -> PackageDeps -> PackageDeps
removeSubmodule' dep (PackageDeps ss ls) =
  PackageDeps (Set.delete dep ss) ls

------------------------------------------------------------------------

bracketDirectory :: IO a -> IO a
bracketDirectory io = bracket getCurrentDirectory setCurrentDirectory (const io)

withTempDirectory :: Testable a => (Directory -> EitherT ProcessError IO a) -> Property
withTempDirectory io = testIO . bracketDirectory $ do
  result  <- withSystemTempDirectory "mafia.chaos" (runEitherT . io . T.pack)
  case result of
    Left  err -> return (QC.counterexample (show err) False)
    Right x   -> return (property x)

hush :: (MonadIO m, MonadCatch m) => a -> m a -> m a
hush def = handle $ \(_ :: IOException) -> return def

------------------------------------------------------------------------

writeRepo :: (MonadIO m, MonadCatch m) => Directory -> Repo -> m ()
writeRepo dir repo@(Repo focus others) = do
  writePackage dir repo focusPackageName focus
  mapM_ (\(name, pkg) -> writePackage dir repo name pkg) (Map.toList others)

  let lib = dir </> "lib"
  mapM_ (\name -> writePackage lib repo name emptyPackageDeps) (repoSubmodules repo)

writePackage :: (MonadIO m, MonadCatch m) => Directory -> Repo -> PackageName -> PackageDeps -> m ()
writePackage dir repo name pd@(PackageDeps ss ls) = do
  let deps   = Set.toList (Set.union ss ls)
      locals = Set.toList (recursiveLocals repo pd)
  writeProject (dir </> name) name deps locals

writeProject :: (MonadIO m, MonadCatch m) => Directory -> Text -> [Text] -> [Text] -> m ()
writeProject dir name deps locals = do
  let modName = dromedary name
      cabalFile = dir </> name <> ".cabal"
      subsFile  = dir </> name <> ".submodules"
      hsFile    = dir </> "src" </> modName <> ".hs"

  writeFile cabalFile (cabalText name deps)
  writeFile hsFile    ("module " <> modName <> " where\n")

  if null locals
     then removeFile subsFile `catch` \(_ :: IOException) -> return ()
     else writeFile  subsFile (T.unlines locals)

writeFile :: MonadIO m => Path -> Text -> m ()
writeFile path txt = do
  createDirectoryIfMissing True (takeDirectory path)
  writeText path txt

dromedary :: Text -> Text
dromedary = mconcat . fmap upcase . T.splitOn "-"
  where
    upcase xs | T.null xs = xs
              | otherwise = T.singleton (toUpper (T.head xs)) <> T.tail xs

cabalText :: Text -> [Text] -> Text
cabalText name deps = T.unlines [
    "name:          acme-" <> name
  , "version:       0.0.1"
  , "license:       AllRightsReserved"
  , "author:        Mafia Chaos"
  , "maintainer:    Mafia Chaos"
  , "synopsis:      synopsis"
  , "category:      Development"
  , "cabal-version: >= 1.8"
  , "build-type:    Simple"
  , "description:   description"
  , ""
  , "library"
  , "  hs-source-dirs: src"
  , "  build-depends:"
  , "      base >= 3 && < 5"
  ] <> T.unlines (fmap ("    , acme-" <>) deps)

------------------------------------------------------------------------

prop_chaos (Actions actions) = withTempDirectory $ \temp -> do
  liftIO (print actions)

  mafia <- (</> "dist/build/mafia/mafia") <$> getCurrentDirectory

  let repoDir   = temp </> "repo"
      focusDir  = repoDir </> focusPackageName

  createDirectoryIfMissing False repoDir
  createDirectoryIfMissing False focusDir

  setCurrentDirectory repoDir
  call_ id "git" ["init"]

  setCurrentDirectory focusDir
  writeRepo repoDir emptyRepo

  case fromActions actions of
    Nothing -> do
      return (QC.counterexample "Invalid set of actions" (property False))

    Just testRun -> do
      forM_ testRun $ \(action, repo) -> do
        runAction mafia repoDir repo action
      return (property True)

runAction :: File -> Directory -> Repo -> Action -> EitherT ProcessError IO ()
runAction mafia repoDir repo = \case
  Build -> do
    liftIO (T.putStrLn "$ mafia build")
    call_ id mafia ["build"]

  AddLocal dep pkg -> do
    putAdd (dep <> " -> " <> pkg)
    writeRepo repoDir repo

  AddSubmodule dep pkg -> do
    let dep' = "lib" </> dep
    putAdd (dep' <> " -> " <> pkg)
    writeRepo repoDir repo

  RemoveLocal dep -> do
    putRemove dep
    removeDirectoryRecursive (repoDir </> dep)
    writeRepo repoDir repo

  RemoveSubmodule dep -> do
    let dep' = "lib" </> dep
    putRemove dep'
    removeDirectoryRecursive (repoDir </> dep')
    writeRepo repoDir repo

  SidestepLocal dep -> do
    putSidestep dep
    removeDirectoryRecursive (repoDir </> dep)
    _ <- liftIO . runEitherT $ call_ id mafia ["build"]
    writeRepo repoDir repo

  SidestepSubmodule dep -> do
    let dep' = "lib" </> dep
    putSidestep dep'
    removeDirectoryRecursive (repoDir </> dep')
    _ <- liftIO . runEitherT $ call_ id mafia ["build"]
    writeRepo repoDir repo

putAdd :: MonadIO m => Text -> m ()
putAdd x = liftIO (T.putStrLn ("+ " <> x))

putRemove :: MonadIO m => Text -> m ()
putRemove x = liftIO (T.putStrLn ("- " <> x))

putSidestep :: MonadIO m => Text -> m ()
putSidestep x = liftIO (T.putStrLn ("~ " <> x))

------------------------------------------------------------------------

return []
tests = $forAllProperties $ QC.quickCheckWithResult (QC.stdArgs {QC.maxSuccess = 10})
