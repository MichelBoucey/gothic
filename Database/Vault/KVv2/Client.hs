{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Vault.KVv2.Client (

  -- * Connection and configure Vault KV v2 Engine
  vaultConnect,
  configKVEngine,

  -- * Basic operations
  getSecret,
  putSecret,

  -- * Soft secret deletion
  deleteSecret,
  deleteSecretVersions,
  unDeleteSecretVersions,

  -- * Permanent secret deletion
  destroySecret,
  destroySecretVersions,

  -- * Get infomation
  readSecretMetadata,
  secretsList,

  -- * Utils
  secret,
  toSecretData,
  toSecretVersions,

) where

import           Control.Lens
import qualified Data.Aeson                          as A
import           Data.Aeson.Lens
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as C
import qualified Data.Text                           as T
import qualified Data.HashMap.Strict                 as HM
import qualified Data.Maybe                          as M
import qualified Data.Vector                         as V (fromList)
import           Network.Connection 
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment                  (lookupEnv)
import           System.Posix.Files                  (fileExist)

import           Database.Vault.KVv2.Client.Internal
import           Database.Vault.KVv2.Client.Types


--https://www.vaultproject.io/api/secret/kv/kv-v2.html

-- | Get a 'VaultConnection' or an error message.
vaultConnect :: Maybe String                       -- ^ Use Just this string as Vault address or get it from VAULT_ADDR
             -> String                             -- ^ Secrets engine path
             -> Maybe VaultToken                   -- ^ Use Just this Vault token or get it from $HOME/.vaut-token
             -> Bool                               -- ^ Disable certificate validation
             -> IO (Either String VaultConnection)
vaultConnect mva sep mvt dcv = do
  nm <- newTlsManagerWith $
          mkManagerSettings
            TLSSettingsSimple
              { settingDisableCertificateValidation = dcv
              , settingDisableSession               = False
              , settingUseServerName                = True
              }
            Nothing
  va <- case mva of
          Just va -> return (Just va)
          Nothing -> lookupEnv "VAULT_ADDR"
  evt <- case mvt of
           Just t  -> return (Right $ C.pack t)
           Nothing -> do
             hm <- lookupEnv "HOME"
             if M.isJust hm
               then do
                 let pf = M.fromJust hm ++ "/.vault-token"
                 if M.isJust va
                   then do
                     fe <- fileExist pf
                     if fe 
                       then Right <$> B.readFile pf
                       else return (Left $ "No Vault token file found at " ++ pf)
                   else return (Left "Variable environnment VAULT_ADDR not set")
               else return (Left "Variable environnment VAULT_ADDR not set")
  pure $
    (\vt -> VaultConnection { vaultAddr = M.fromJust va, vaultToken = vt, secretsEnginePath = sep, manager = nm }) <$> evt

configKVEngine :: Int  -- ^ Max versions
               -> Bool -- ^ CAS required
               -> IO (Either String A.Value)
configKVEngine = undefined  -- TODO -> KVv2Config type

-- | Get a secret from Vault.
getSecret :: VaultConnection
          -> SecretPath
          -> SecretVersion
          -> IO (Either String A.Value)
getSecret VaultConnection{..} (SecretPath sp) sv = do
  request <-
    parseRequest $
      concat [ vaultAddr, "/v1/", secretsEnginePath, "data/", sp, queryString sv ]
  runRequest request { requestHeaders = vaultHeaders vaultToken } manager
  where
    queryString LatestVersion = mempty
    queryString (Version v)   = "?version=" ++ show v

-- | Put a secret in Vault.
putSecret :: VaultConnection
          -> CheckAndSet
          -> SecretPath
          -> SecretData
          -> IO (Either String A.Value)
putSecret VaultConnection{..} cas (SecretPath sp) sd = do
  let rb = RequestBodyLBS $ A.encode PutSecretRequestBody { options = PutSecretOptions { cas = cas }, put_data = sd }
  request <-
    parseRequest $
      concat [ "POST ", vaultAddr, "/v1/", secretsEnginePath, "data/", sp ]
  runRequest request { requestHeaders = vaultHeaders vaultToken, requestBody = rb } manager

{- 
putSecret -> (Version 1.0)
Object (fromList [("lease_duration",Number 0.0),("wrap_info",Null),("auth",Null),("data",Object (fromList [("destroyed",Bool False),("deletion_time",String ""),("version",Number 1.0),("created_time",String "2019-05-14T20:54:20.519889313Z")])),("request_id",String "52c3a8a0-1409-c41c-ba94-6eab8a4e4420"),("warnings",Null),("lease_id",String ""),("renewable",Bool False)])


putSecret conn CreateOnly (SecretPath "Bob") (toSecretData [("Square","PANTS")])
Right (Object (fromList [("errors",Array [String "check-and-set parameter did not match the current version"])]))
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
      , requestBody = RequestBodyLBS (A.encode vs)
      }
    manager

destroySecret :: VaultConnection
              -> SecretPath
              -> IO (Either String A.Value)
destroySecret VaultConnection{..} (SecretPath sp) = do
  request <- parseRequest $ concat [ "DELETE ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp ]
  runRequest request { requestHeaders = vaultHeaders vaultToken } manager

destroySecretVersions :: VaultConnection
                      -> SecretPath
                      -> SecretVersions
                      -> IO (Either String A.Value)
destroySecretVersions VaultConnection{..} (SecretPath sp) vs = do
  request <- parseRequest $ concat [ "POST ", vaultAddr, "/v1/", secretsEnginePath, "destroy/", sp ]
  runRequest
    request
      { requestHeaders = vaultHeaders vaultToken
      , requestBody = RequestBodyLBS (A.encode vs)
      }
    manager

secretsList :: VaultConnection
            -> SecretPath
            -> IO (Either String A.Value)
secretsList VaultConnection{..} (SecretPath sp) = do
  request <- parseRequest $ concat [ "LIST ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp ]
  runRequest request { requestHeaders = vaultHeaders vaultToken } manager

readSecretMetadata :: VaultConnection
            -> SecretPath
            -> IO (Either String A.Value)
readSecretMetadata VaultConnection{..} (SecretPath sp) = do
  request <- parseRequest $ concat [ "GET ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp ]
  runRequest request { requestHeaders = vaultHeaders vaultToken } manager

-- updateSecretMetadata = undefined

-- Utils

secret :: Either String A.Value
       -> IO (Either String SecretData)
secret (Left s) = return (Left s)
secret (Right v) =
 return $
   case v ^? key "data" . key "data" of
     Just o  ->
       case A.fromJSON o :: A.Result SecretData of
         A.Success sd -> Right sd
         A.Error e    -> Left e
     Nothing -> Left "No secret data"

toSecretData :: [(T.Text,T.Text)] -> SecretData
toSecretData l = SecretData (HM.fromList l)

toSecretVersions :: [Int] -> SecretVersions
toSecretVersions is = SecretVersions $ V.fromList $ A.toJSON <$> is
{-
isDestroyed :: Either String A.Value -> IO (Either String Bool)
isDestroyed = undefined
-}
