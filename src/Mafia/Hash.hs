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

import           Crypto.Hash (Digest, SHA1)
import qualified Crypto.Hash as Hash

import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import           Data.ByteArray (ByteArrayAccess, convert)
import           Data.ByteArray.Encoding (Base(..), convertToBase)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Mafia.IO
import           Mafia.Path

import           Mafia.P

import           System.IO (IO)

import           Control.Monad.Trans.Either (EitherT, left)

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
      unHash :: Digest SHA1
    } deriving (Eq, Ord, Show)

renderHash :: Hash -> Text
renderHash =
  T.decodeUtf8 . takeBase16 . unHash

parseHash :: Text -> Maybe Hash
parseHash hex =
  case Base16.decode $ T.encodeUtf8 hex of
    (bs, "") ->
      Hash <$> Hash.digestFromByteString bs
    _ ->
      Nothing

------------------------------------------------------------------------

hashHashes :: [Hash] -> Hash
hashHashes =
  Hash . Hash.hashlazy . L.fromChunks . fmap (takeBytes . unHash)

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
  Hash (Hash.hash bytes)

------------------------------------------------------------------------

takeBytes :: ByteArrayAccess a => a -> ByteString
takeBytes =
  convert

takeBase16 :: ByteArrayAccess a => a -> ByteString
takeBase16 =
  convertToBase Base16

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
