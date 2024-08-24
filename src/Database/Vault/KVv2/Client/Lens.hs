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
import qualified Data.Aeson                          as A
import           Data.Aeson.Lens
import qualified Data.Text                           as T
import qualified Data.Vector                         as V

import           Database.Vault.KVv2.Client.Internal
import           Database.Vault.KVv2.Client.Types

secret
  :: A.Value
  -> Either String SecretData
secret =
  fromVaultResponse "data" toSecretData
  where
  toSecretData o@(A.Object _) =
    case A.fromJSON o of
      A.Success sd -> Right sd
      A.Error e    -> Left e
  toSecretData A.Null         = Left "No current secret version"
  toSecretData _              = Left "Expected JSON object"

version
  :: A.Value
  -> Either String SecretVersion
version =
  fromVaultResponse "version" toSecretVersion
  where
  toSecretVersion (A.Number n) = Right (SecretVersion $ toInt n)
  toSecretVersion _            = Left "Expected JSON number"

current
  :: A.Value
  -> Either String SecretVersion
current =
  fromVaultResponse "current_version" toSecretVersion
  where
  toSecretVersion (A.Number n) = Right (SecretVersion $ toInt n)
  toSecretVersion _            = Left "Expected JSON number"

metadata
  :: A.Value
  -> Either String SecretMetadata
metadata =
  fromVaultResponse "versions" toSecretMetadata
  where
  toSecretMetadata o@(A.Object _) =
    case A.fromJSON o of
      A.Success vs -> Right vs
      A.Error e    -> Left e
  toSecretMetadata _              = error "Expected JSON object"

list
  :: A.Value
  -> Either String [VaultKey]
list =
  fromVaultResponse "keys" toListKeys
  where
    toListKeys (A.Array a) =
      Right (V.foldl lks mempty a)
      where
        lks ks (A.String t) =
          let s = T.unpack t in
          (if hasTrailingSlash s
             then VaultFolder s
             else VaultKey s) : ks
        lks p       _       = p
    toListKeys _            = error "Expected JSON array"

maybeError
  :: Either String A.Value
  -> Maybe Error
maybeError (Left s)  = Just s
maybeError (Right v) =
  case v ^? key "data" . key "version" of
    Just A.Null -> Nothing
    Just _      -> Just "Unexpected JSON type"
    Nothing     -> Just (jsonErrors v)

