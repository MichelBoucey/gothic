{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase #-}

module Database.Vault.KVv2.Client (

  readSecret,
  putSecret,
  deleteSecret,
  toSecretData

) where

import           Control.Monad.Catch
import qualified Data.ByteString           as B
import qualified Data.Aeson                as A
import qualified Data.Text                 as T
import           Data.HashMap.Strict
import           Network.HTTP.Client
import           Network.HTTP.Types.Header

import Database.Vault.KVv2.Client.Types

readSecret :: VaultConnection -> SecretPath -> SecretVersion -> IO (Maybe A.Value)
readSecret VaultConnection { config = VaultConfig {..}, .. } (SecretPath sp) sv = do
  request <- parseRequest $
               concat [ vaultAddr, "/v1/", secretsEnginePath, "data/", sp, queryString sv ]
  runRequest request { requestHeaders = vaultHeaders vaultToken } manager
  where
    queryString LatestVersion = mempty
    queryString (Version v)   = "?version=" ++ show v

putSecret :: VaultConnection -> CheckAndSet -> SecretPath -> SecretData -> IO (Maybe A.Value)
putSecret VaultConnection { config = VaultConfig {..}, .. } cas (SecretPath sp) sd = do
  let rb = RequestBodyLBS $
             A.encode PutSecretRequestBody { options = PutSecretOptions { cas = cas } , secret_data = sd }
  request <- parseRequest $ concat [ "POST ", vaultAddr, "/v1/", secretsEnginePath, "data/", sp ]
  runRequest request { requestHeaders = vaultHeaders vaultToken, requestBody = rb } manager

deleteSecret :: VaultConnection -> SecretPath -> IO (Maybe A.Value)
deleteSecret VaultConnection { config = VaultConfig {..}, .. } (SecretPath sp) = do
  request <- parseRequest $ concat [ "DELETE ", vaultAddr, "/v1/", secretsEnginePath, "data/", sp ]
  runRequest request { requestHeaders = vaultHeaders vaultToken } manager

{-
-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#delete-secret-versions
deleteSecretVersions = undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#undelete-secret-versions
undeleteSecretVersions = undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#destroy-secret-versions
destroySecretVersions =undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#list-secrets
secretsList = undefined
-}

{-
--https://www.vaultproject.io/api/secret/kv/kv-v2.html#read-secret-metadata
readSecretMetadata = undefined

updateSecretMetadata = undefined

-}

runRequest :: Request -> Manager -> IO (Maybe A.Value)
runRequest r m =
  try (httpLbs r m) >>=
    \case 
      Right b -> return (A.decode $ responseBody b)
      Left  e -> return (Just $ A.object ["error" A..= T.pack (show (e :: SomeException))])

vaultHeaders :: B.ByteString -> [Header]
vaultHeaders vt =
  [ ("Content-Type", "application/json; charset=utf-8")
  , ("X-Vault-Token", vt) ]

toSecretData :: [(T.Text, T.Text)] -> SecretData
toSecretData l = SecretData (fromList l)
