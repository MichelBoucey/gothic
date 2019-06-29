{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Lens (

    current,
    list,
    metadata,
    maybeError,
    secret,
    version

  ) where

import           Control.Lens
import qualified Data.Aeson                       as A
import           Data.Aeson.Lens
import           Data.Text                        as T
import qualified Data.Vector                      as V

import           Database.Vault.KVv2.Client.Internal
import           Database.Vault.KVv2.Client.Types

secret
  :: Either String A.Value
  -> Either String SecretData
secret (Left s) = Left s
secret (Right v) =
  case v ^? key "data" . key "data" of
    Just o  ->
      case o of
        A.Null -> Left "No current secret version"
        A.Object _ ->
          case A.fromJSON o of
            A.Success sd -> Right sd
            A.Error e    -> Left e
        _          -> Left "Unexpected JSON type"
    Nothing -> Left "No secret data"

version
  :: Either String A.Value
  -> Either String SecretVersion
version (Left s) = Left s
version (Right v) =
  case v ^? key "data" . key "version" of
    Just (A.Number n) -> Right (SecretVersion $ toInt n)
    Just _            -> Left "Unexpected JSON type"
    Nothing           -> Left "No secret version"

maybeError
  :: Either String A.Value
  -> Maybe Error
maybeError (Left s) = Just s
maybeError (Right v) =
  case v ^? key "data" . key "version" of
    Just A.Null -> Nothing
    Just _      -> Just "Unexpected JSON type"
    Nothing     -> Just "No secret version JSON field"

current
  :: Either String A.Value
  -> Either String SecretVersion
current (Left s) = Left s
current (Right v) =
  case v ^? key "data" . key "current_version" of
    Just (A.Number n) -> Right (SecretVersion $ toInt n)
    Just _            -> Left "Unexpected JSON type"
    Nothing           -> Left "No current secret version JSON field"

metadata
  :: Either String A.Value
  -> Either String SecretMetadata
metadata (Left s) = Left s
metadata (Right v) =
  case v ^? key "data" . key "versions" of
    Just o@(A.Object _) ->
      case A.fromJSON o of
        A.Success vs -> Right vs
        A.Error e    -> Left e
    Just _           -> Left "Unexpected JSON type"
    Nothing             -> Left "No secret versions JSON field"

list
  :: Either String A.Value
  -> Either String [VaultKey]
list (Left s) = Left s
list (Right v) =
  case v ^? key "data" . key "keys" of
    Just (A.Array a)  -> Right (listKeys a)
    Just _            -> Left "Unexpected JSON type"
    Nothing           -> Left "No secret keys JSON field"
  where
  listKeys =
    V.foldl lks mempty
    where
      lks ks (A.String t) =
        if T.last t == '/'
          then ks <> [VaultFolder t]
          else ks <> [VaultKey t]
      lks p       _       = p
 
