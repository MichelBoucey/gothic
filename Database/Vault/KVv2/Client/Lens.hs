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
  :: Either String A.Value
  -> Either String SecretData
secret e =
  e >>= \v ->
    case v ^? key "data" . key "data" of
      Just o  ->
        case o of
          A.Null -> Left "No current secret version"
          A.Object _ ->
            case A.fromJSON o of
              A.Success sd -> pure sd
              A.Error f    -> Left f
          _          -> Left "Unexpected JSON type"
      Nothing -> fail (jsonErrors v)

version
  :: Either String A.Value
  -> Either String SecretVersion
version e =
  e >>= \v ->
    case v ^? key "data" . key "version" of
      Just (A.Number n) -> pure (SecretVersion $ toInt n)
      Just _            -> unexpectedJSONType
      Nothing           -> Left (jsonErrors v)

maybeError
  :: Either String A.Value
  -> Maybe Error
maybeError (Left s)  = Just s
maybeError (Right v) =
  case v ^? key "data" . key "version" of
    Just A.Null -> Nothing
    Just _      -> Just "Unexpected JSON type"
    Nothing     -> Just (jsonErrors v)

current
  :: Either String A.Value
  -> Either String SecretVersion
current e =
  e >>= \v ->
    case v ^? key "data" . key "current_version" of
      Just (A.Number n) -> pure (SecretVersion $ toInt n)
      Just _            -> unexpectedJSONType
      Nothing           -> Left (jsonErrors v)

metadata
  :: Either String A.Value
  -> Either String SecretMetadata
metadata e =
  e >>= \v ->
    case v ^? key "data" . key "versions" of
      Just o@(A.Object _) ->
        case A.fromJSON o of
          A.Success vs -> Right vs
          A.Error f    -> Left f
      Just _           -> unexpectedJSONType
      Nothing          -> Left (jsonErrors v)

list
  :: Either String A.Value
  -> Either String [VaultKey]
list e =
  e >>= \v ->
    case v ^? key "data" . key "keys" of
      Just (A.Array a) -> Right (listKeys a)
      Just _           -> unexpectedJSONType
      Nothing          -> Left (jsonErrors v)
    where
    listKeys =
      V.foldl lks mempty
      where
        lks ks (A.String t) =
          let s = T.unpack t in
          (if hasTrailingSlash s
             then VaultFolder s
             else VaultKey s) : ks
        lks p       _       = p
 
