{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Vault.KV.V2 (

  kvReadSecretVersion

) where

import Network.HTTP.Client
import Network.HTTP.Types.Header
import qualified Data.ByteString as B

import Database.Vault.KV.V2.Types

-- https://haskell-lang.org/library/http-client
{-
{
  "data": {
    "data": {
        "foo": "bar"
    },
    "metadata": {
      "created_time": "2018-03-22T02:24:06.945319214Z",
      "deletion_time": "",
      "destroyed": false,
      "version": 1
    }
  },
}
-}

kvReadSecretVersion VaultConnection{config = VaultConfig{..},..} (SecretPath p) LatestVersion = do
  req <- parseRequest (vaultAddr ++ "/v1/" ++ secretsEnginePath ++ "data/" ++ p)
         >>= \r -> return r { requestHeaders = vaultRequestHeaders vaultToken }
  httpLbs req manager
kvReadSecretVersion VaultConnection{config = VaultConfig{..},..} (SecretPath p) (Version v) = do
  req <- parseRequest (vaultAddr ++ "/v1/" ++ secretsEnginePath ++ "data/" ++ p ++ "?version=" ++ (show v))
         >>= \r -> return r { requestHeaders = vaultRequestHeaders vaultToken }
  httpLbs req manager

{-

vaultAPIRequest :: Method 

kvPut = undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#create-update-secret
kvSecretNewVersion = undefined

kvSecretDelete = undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#delete-secret-versions
kvDeleteSecretVersions = undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#undelete-secret-versions
kvUndeleteSecretVersions = undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#destroy-secret-versions
kvDestroySecretVersions =undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#list-secrets
kvListSecrets = undefined

--https://www.vaultproject.io/api/secret/kv/kv-v2.html#read-secret-metadata
kvReadSecretMetadata = undefined

kvUpdateSecretMetadata = undefined

-}

vaultRequestHeaders :: B.ByteString -> [Header]
vaultRequestHeaders vt =
  [ ("Content-Type", "application/json; charset=utf-8")
  , ("X-Vault-Token", vt)
  ]

