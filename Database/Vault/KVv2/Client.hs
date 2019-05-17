{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Vault.KVv2.Client (

  vaultConnect,
  getSecret,
  putSecret,
  deleteSecret,
  deleteSecretVersions,
  unDeleteSecretVersions,
  destroySecretVersions,
  secretsList,
  toSecretData,
  toSecretVersions,
  secretData

) where

import qualified Data.Aeson                          as A
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as C
import qualified Data.Text                           as T
import           Data.HashMap.Strict                 as HM
import           Data.Vector                         as V (fromList)
import           Network.Connection 
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment                  (lookupEnv)
import           System.Posix.Files                  (fileExist)

import           Database.Vault.KVv2.Client.Internal
import           Database.Vault.KVv2.Client.Types

vaultConnect :: String                             -- ^ Secrets engine path
             -> Maybe VaultToken
             -> Bool                               -- ^ Certificate validation
             -> IO (Either String VaultConnection)
vaultConnect sep mvt dcv = do
  nm <- newTlsManagerWith $
         mkManagerSettings
           TLSSettingsSimple
             { settingDisableCertificateValidation = dcv
             , settingDisableSession               = False
             , settingUseServerName                = True }
           Nothing
  Just va <- lookupEnv "VAULT_ADDR"
  evt <- case mvt of
             Just t  -> return $ Right (C.pack t)
             Nothing -> do
               Just hm <- lookupEnv "HOME" -- TODO
               let pf = hm ++ "/.vault-token"
               fe <- fileExist pf
               if fe
                 then Right <$> B.readFile pf
                 else return (Left $ "No Vault token file found at " ++ pf)
  return $
    case evt of
      Right vt -> Right VaultConnection { vaultAddr = va, vaultToken = vt, secretsEnginePath = sep, manager = nm }
      Left s   -> Left s

-- configSecretEngine

getSecret :: VaultConnection
          -> SecretPath
          -> SecretVersion
          -> IO (Either String VaultResponse)
getSecret VaultConnection{..} (SecretPath sp) sv = do
  request <- parseRequest $
               concat [ vaultAddr, "/v1/", secretsEnginePath, "data/", sp, queryString sv ]
  esv <- runRequest request { requestHeaders = vaultHeaders vaultToken } manager
  return $
    case esv of
      Right v ->
       case A.fromJSON v :: A.Result VaultResponse of
         A.Success vr -> Right vr
         A.Error e    -> Left e
      Left e          -> Left e
  where
    queryString LatestVersion = mempty
    queryString (Version v)   = "?version=" ++ show v

putSecret :: VaultConnection
          -> CheckAndSet
          -> SecretPath
          -> SecretData
          -> IO (Either String A.Value)
putSecret VaultConnection{..} cas (SecretPath sp) sd = do
  let rb = RequestBodyLBS $
             A.encode PutSecretRequestBody { options = PutSecretOptions { cas = cas }, put_data = sd }
  request <- parseRequest $ concat [ "POST ", vaultAddr, "/v1/", secretsEnginePath, "data/", sp ]
  runRequest request { requestHeaders = vaultHeaders vaultToken, requestBody = rb } manager

{-
Object (fromList [("lease_duration",Number 0.0),("wrap_info",Null),("auth",Null),("data",Object (fromList [("destroyed",Bool False),("deletion_time",String ""),("version",Number 1.0),("created_time",String "2019-05-14T20:54:20.519889313Z")])),("request_id",String "52c3a8a0-1409-c41c-ba94-6eab8a4e4420"),("warnings",Null),("lease_id",String ""),("renewable",Bool False)])
-}

deleteSecret :: VaultConnection
             -> SecretPath
             -> IO (Either String A.Value)
deleteSecret VaultConnection{..} (SecretPath sp) = do
  request <- parseRequest $ concat [ "DELETE ", vaultAddr, "/v1/", secretsEnginePath, "data/", sp ]
  runRequest request { requestHeaders = vaultHeaders vaultToken } manager

deleteSecretVersions :: VaultConnection
                     -> SecretPath
                     -> SecretVersions
                     -> IO (Either String A.Value)
deleteSecretVersions VaultConnection{..} (SecretPath sp) vs = do
  request <- parseRequest $ concat [ "POST ", vaultAddr, "/v1/", secretsEnginePath, "delete/", sp ]
  runRequest request { requestHeaders = vaultHeaders vaultToken, requestBody = RequestBodyLBS (A.encode vs) } manager

unDeleteSecretVersions :: VaultConnection
                       -> SecretPath
                       -> SecretVersions
                       -> IO (Either String A.Value)
unDeleteSecretVersions VaultConnection{..} (SecretPath sp) vs = do
  request <- parseRequest $ concat [ "POST ", vaultAddr, "/v1/", secretsEnginePath, "undelete/", sp ]
  runRequest
    request
      { requestHeaders = vaultHeaders vaultToken
      , requestBody = RequestBodyLBS (A.encode vs) }
    manager

destroySecretVersions :: VaultConnection
                      -> SecretPath
                      -> SecretVersions
                      -> IO (Either String A.Value)
destroySecretVersions VaultConnection{..} (SecretPath sp) vs = do
  request <- parseRequest $ concat [ "POST ", vaultAddr, "/v1/", secretsEnginePath, "destroy/", sp ]
  runRequest
    request
      { requestHeaders = vaultHeaders vaultToken
      , requestBody = RequestBodyLBS (A.encode vs) }
    manager

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html#list-secrets
-- secretsList = undefined
secretsList :: VaultConnection
            -> SecretPath
            -> IO (Either String A.Value)
secretsList VaultConnection{..} (SecretPath sp) = do
  request <- parseRequest $ concat [ "LIST ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp ]
  runRequest request { requestHeaders = vaultHeaders vaultToken } manager

--https://www.vaultproject.io/api/secret/kv/kv-v2.html#read-secret-metadata
-- readSecretMetadata = undefined

-- updateSecretMetadata = undefined

-- Utils

secretData :: VaultResponse -> SecretData
secretData VaultResponse{response_data=ResponseData{secret_data=sd}} = sd

toSecretData :: [(T.Text, T.Text)] -> SecretData
toSecretData l = SecretData (HM.fromList l)

toSecretVersions :: [Int] -> SecretVersions
toSecretVersions is = SecretVersions $ V.fromList $ A.toJSON <$> is

