{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Lens (

    secret,
    version,

  ) where

import           Control.Lens
import qualified Data.Aeson                       as A
import           Data.Aeson.Lens
import           Data.Scientific
-- import           Data.Text                        as T hiding (foldl)

import           Database.Vault.KVv2.Client.Types

-- TODO -> getSecretR "*** Exception: expected HashMap ~Text v, encountered Null" after deleteSecretR (latestVersion)
secret
  :: Either String A.Value
  -> Either String SecretData
secret (Left s) = fail s
secret (Right v) =
  case v ^? key "data" . key "data" of
    Just o  ->
      case A.fromJSON o of
        A.Success sd -> Right sd -- TODO -> case Value == Null -> fail
        A.Error e    -> Left e
    Nothing -> Left "No secret data JSON object"

version
  :: Either String A.Value
  -> Either String SecretVersion
version (Left s) = fail s
version (Right v) =
  case v ^? key "data" . key "version" of
    Just (A.Number n) -> return (Version $ base10Exponent n)
    Just _            -> fail "No secret version JSON field"
    Nothing           -> fail "No secret version JSON field"

{-
listKeys :: [A.Value] -> ([VaultKey],[VaultKey])
listKeys =
  foldl lks ([],[])
  where
    lks (ks,fs) (A.String t) =
      if T.last t == '/'
        then (ks,fs++[VaultFolder t])
        else (ks++[VaultKey t],fs)
    lks p       _          = p
-}
