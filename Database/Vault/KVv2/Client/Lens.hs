{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Lens (

    secret

  ) where

import           Control.Lens
import qualified Data.Aeson                       as A
import           Data.Aeson.Lens

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
        A.Success sd -> return sd --case Value == Null -> fail
        A.Error e    -> fail e
    Nothing -> fail "Not a secret data JSON object"

