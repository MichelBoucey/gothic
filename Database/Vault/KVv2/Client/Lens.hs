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
secret (Left s) = fail s
secret (Right v) =
  case v ^? key "data" . key "data" of
    Just o  ->
      case A.fromJSON o of
        A.Success sd -> Right sd
        A.Error e    -> Left e   -- TODO -> Left "expected HashMap ~Text v, encountered Null"
    Nothing -> Left "No secret data JSON object"

version
  :: Either String A.Value
  -> Either String SecretVersion
version (Left s) = fail s
version (Right v) =
  case v ^? key "data" . key "version" of
    Just (A.Number n) -> return (SecretVersion $ toInt n)
    Just _            -> fail "No secret version JSON field"
    Nothing           -> fail "No secret version JSON field"

maybeError
  :: Either String A.Value
  -> Maybe Error
maybeError (Left s) = return s
maybeError (Right v) =
  case v ^? key "data" . key "version" of
    Just A.Null -> Nothing
    Just _      -> return "No secret version JSON field"
    Nothing     -> return "No secret version JSON field"

current
  :: Either String A.Value
  -> Either String SecretVersion
current (Left s) = fail s
current (Right v) =
  case v ^? key "data" . key "current_version" of
    Just (A.Number n) -> return (SecretVersion $ toInt n)
    Just _            -> fail "No current secret version JSON field"
    Nothing           -> fail "No current secret version JSON field"

metadata
  :: Either String A.Value
  -> Either String SecretMetadata
metadata (Left s) = fail s
metadata (Right v) =
  case v ^? key "data" . key "versions" of
    Just o@(A.Object _) ->
      case A.fromJSON o of
        A.Success vs  -> return vs
        A.Error e    -> Left e
    Just _            -> fail "No secret version JSON field"
    Nothing           -> fail "No secret version JSON field"

list
  :: Either String A.Value
  -> Either String [VaultKey]
list (Left s) = fail s
list (Right v) =
  case v ^? key "data" . key "keys" of
    Just (A.Array a)  -> return (listKeys a)
    Just _            -> Left "No secret data JSON object"
    Nothing           -> Left "No secret data JSON object"
  where
  listKeys =
    V.foldl lks mempty
    where
      lks ks (A.String t) =
        if T.last t == '/'
          then ks <> [VaultFolder t]
          else ks <> [VaultKey t]
      lks p       _       = p
 
