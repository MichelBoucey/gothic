{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Vault.KVv2.Client (

  readSecret,
  putSecret,
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

readSecret :: VaultConnection
           -> SecretPath
           -> SecretVersion
           -> IO (Maybe A.Value) -- IO (Either VaultResponse)
readSecret VaultConnection { config = VaultConfig {..}, .. } (SecretPath sp) sv = do
  let url = vaultAddr ++ "/v1/" ++ secretsEnginePath ++ "data/" ++ sp ++ version sv
  req <- parseRequest url >>= \r -> return r { requestHeaders = vaultRequestHeaders vaultToken }
  t <- try (httpLbs req manager)
  return $ case t of
    Right r -> A.decode (responseBody r)
    Left  e -> Just $ A.object ["error" A..= T.pack (show (e :: SomeException))]
  where
  version LatestVersion = mempty 
  version (Version v)   = "?version=" ++ show v

putSecret :: VaultConnection
          -> CheckAndSet
          -> SecretPath
          -> SecretData
          -> IO (Maybe A.Value)
putSecret VaultConnection { config = VaultConfig {..}, .. } cas (SecretPath sp) sd = do
  let rb = RequestBodyLBS $ A.encode PutSecretRequestBody { options = PutSecretOptions { cas = cas }, secret_data = sd }
  req <- parseRequest (vaultAddr ++ "/v1/" ++ secretsEnginePath ++ "data/" ++ sp) >>= \c ->
         return c { method = "POST", requestHeaders = vaultRequestHeaders vaultToken, requestBody = rb }
  t <- try (httpLbs req manager)
  return $ case t of
    Right r -> A.decode $ responseBody r
    Left  e -> Just $ A.object ["error" A..= T.pack (show (e :: SomeException))]

{- putSecret OK rsponse:
Just (Object (fromList [("lease_duration",Number 0.0),("wrap_info",Null),("auth",Null),("data",Object (fromList [("destroyed",Bool False),("deletion_time",String ""),("version",Number 1.0),("created_time",String "2019-05-05T19:01:25.03675828Z")])),("request_id",String "43ba19ee-3b89-7153-889c-b82ca74184a4"),("warnings",Null),("lease_id",String ""),("renewable",Bool False)])
-}

{-
-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#delete-latest-version-of-secret
deleteSecret = undefined

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

vaultRequestHeaders :: B.ByteString -> [Header]
vaultRequestHeaders vt =
  [ ("Content-Type", "application/json; charset=utf-8")
  , ("X-Vault-Token", vt)
  ]

toSecretData :: [(T.Text, T.Text)] -> SecretData
toSecretData l = SecretData (fromList l)
