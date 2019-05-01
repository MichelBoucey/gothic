{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Vault.KVv2.Client (

  readSecret

) where

import           Network.HTTP.Client
import           Network.HTTP.Types.Header
import qualified Data.ByteString           as B
import qualified Data.Aeson                as A
import           Control.Monad.Catch
import qualified Data.Text                 as T
import           Control.Monad.IO.Class

import Database.Vault.KVv2.Client.Types
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

readSecret :: VaultConnection
           -> SecretPath
           -> SecretVersion
           -> IO (Maybe A.Value) -- IO (Either VaultResponse)
readSecret VaultConnection{config = VaultConfig{..},..} (SecretPath p) sv = do
  let url = vaultAddr ++ "/v1/" ++ secretsEnginePath ++ "data/" ++ p ++
            (case sv of
               LatestVersion -> mempty
               Version v     -> "?version=" ++ show v)
  req <- parseRequest url >>= \r -> return r { requestHeaders = vaultRequestHeaders vaultToken }
  t <- liftIO (try $ httpLbs req manager)
  return $ case t of
    Right r -> A.decode $ responseBody r
    Left  e -> Just $ A.object ["error" A..= T.pack (show (e :: SomeException))]

{-
putSecret = undefined

vaultAPIRequest :: Method 

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#create-update-secret
newSecretVersion = undefined

deleteSecret = undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#delete-secret-versions
deleteSecretVersions = undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#undelete-secret-versions
undeleteSecretVersions = undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#destroy-secret-versions
destroySecretVersions =undefined

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#list-secrets
secretsList = undefined

--https://www.vaultproject.io/api/secret/kv/kv-v2.html#read-secret-metadata
readSecretMetadata = undefined

updateSecretMetadata = undefined

-}

vaultRequestHeaders :: B.ByteString -> [Header]
vaultRequestHeaders vt =
  [ ("Content-Type", "application/json; charset=utf-8")
  , ("X-Vault-Token", vt)
  ]

