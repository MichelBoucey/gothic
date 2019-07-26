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
import           Data.Text                           as T
import           Data.List                           as L
import qualified Data.Vector                         as V

import           Database.Vault.KVv2.Client.Internal
import           Database.Vault.KVv2.Client.Types

secret
  :: Either String A.Value
  -> Either String SecretData
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
    Nothing ->
      case v ^? key "errors" of
        Just ja ->
          case ja of
            A.Array a ->
              Left $
                L.intercalate
                  ", "
                   (toString <$> V.toList a) ++ "."
            _         -> Left "Unexpected JSON type"
        Nothing -> Left "Expected JSON field: errors"
  where
  toString (A.String s) = T.unpack s
  toString _            = undefined

secret (Left s)  = Left s

version
  :: Either String A.Value
  -> Either String SecretVersion
version (Right v) =
  case v ^? key "data" . key "version" of
    Just (A.Number n) -> Right (SecretVersion $ toInt n)
    Just _            -> Left "Unexpected JSON type"
    Nothing           -> Left "No JSON field \"version\""
version (Left s)  = Left s

maybeError
  :: Either String A.Value
  -> Maybe Error
maybeError (Right v) =
  case v ^? key "data" . key "version" of
    Just A.Null -> Nothing
    Just _      -> Just "Unexpected JSON type"
    Nothing     -> Just "No JSON field \"version\""
maybeError (Left s)  = Just s

current
  :: Either String A.Value
  -> Either String SecretVersion
current (Left s) = Left s
current (Right v) =
  case v ^? key "data" . key "current_version" of
    Just (A.Number n) -> Right (SecretVersion $ toInt n)
    Just _            -> Left "Unexpected JSON type"
    Nothing           -> Left "No JSON field \"current_secret\""

metadata
  :: Either String A.Value
  -> Either String SecretMetadata
metadata (Right v) =
  case v ^? key "data" . key "versions" of
    Just o@(A.Object _) ->
      case A.fromJSON o of
        A.Success vs -> Right vs
        A.Error e    -> Left e
    Just _           -> Left "Unexpected JSON type"
    Nothing          -> Left "No JSON field \"versions\""
metadata (Left s)  = Left s

list
  :: Either String A.Value
  -> Either String [VaultKey]
list (Right v) =
  case v ^? key "data" . key "keys" of
    Just (A.Array a) -> Right (listKeys a)
    Just _           -> Left "Unexpected JSON type"
    Nothing          -> Left "No JSON field \"keys\""
  where
  listKeys =
    V.foldl lks mempty
    where
      lks ks (A.String t) =
        let s = unpack t in
        (if hasTrailingSlash s
           then VaultFolder s
           else VaultKey s) : ks
      lks p       _       = p
list (Left s) = Left s
 
