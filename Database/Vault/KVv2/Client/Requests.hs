{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Vault.KVv2.Client.Requests (

  -- * Connection and configure Vault KV v2 Engine
{-
  vaultConnect,
  kvEngineConfig,
-}
  -- * Basic operations
  getSecretR,
  putSecretR,
  updateSecretMetadataR,

  -- * Soft secret deletion
  deleteSecretR,
  deleteSecretVersionsR,
  unDeleteSecretVersionsR,

  -- * Permanent secret deletion
  destroySecretR,
  destroySecretVersionsR,

  -- * Get information
  readSecretMetadataR,
  secretsListR,

  -- * Utils
--   secret,
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
import           Network.HTTP.Simple                 ( setRequestBody
                                                     , setRequestHeaders
                                                     , setRequestBodyJSON
                                                     )
import           Network.HTTP.Client.TLS
import           System.Environment                  (lookupEnv)
import           System.Posix.Files                  (fileExist)

import           Database.Vault.KVv2.Client.Internal
import           Database.Vault.KVv2.Client.Types
{-
-- | Get a 'VaultConnection' or an error message.
vaultConnect
  :: Maybe String                       -- ^ Use Just this string as Vault address or get it from VAULT_ADDR
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
    (\vt ->
      VaultConnection
        { vaultAddr = M.fromJust va
        , vaultToken = vt
        , secretsEnginePath = sep
        , manager = nm
        }
    ) <$> evt

kvEngineConfig
  :: VaultConnection
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
kvEngineConfig VaultConnection{..} mvs casr =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "config"])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBodyJSON
        SecretSettings
          {  max_versions = mvs
          , cas_required = casr
          }
-}
-- | Get a secret from Vault.
getSecretR
  :: VaultConnection
  -> SecretPath
  -> SecretVersion
  -> IO (Either String A.Value)
getSecretR VaultConnection{..} (SecretPath sp) sv =
  parseRequest (concat [vaultAddr, "/v1/", secretsEnginePath, "data/", sp, queryString sv])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)
  where
    queryString LatestVersion = mempty
    queryString (Version v)   = "?version=" ++ show v

-- | Put a secret in Vault.
putSecretR
  :: VaultConnection
  -> CheckAndSet
  -> SecretPath
  -> SecretData
  -> IO (Either String A.Value)
putSecretR VaultConnection{..} cas (SecretPath sp) sd =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "data/", sp ])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBodyJSON
        PutSecretRequestBody
          { options = PutSecretOptions { cas = cas }
          , put_data = sd
          }

{- 
putSecretR -> (Version 1.0)
Object (fromList [("lease_duration",Number 0.0),("wrap_info",Null),("auth",Null),("data",Object (fromList [("destroyed",Bool False),("deletion_time",String ""),("version",Number 1.0),("created_time",String "2019-05-14T20:54:20.519889313Z")])),("request_id",String "52c3a8a0-1409-c41c-ba94-6eab8a4e4420"),("warnings",Null),("lease_id",String ""),("renewable",Bool False)])


putSecretR conn CreateOnly (SecretPath "Bob") (toSecretData [("Square","PANTS")])
Right (Object (fromList [("errors",Array [String "check-and-set parameter did not match the current version"])]))
-}

deleteSecretR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
deleteSecretR VaultConnection{..} (SecretPath sp) =
  parseRequest (concat ["DELETE ", vaultAddr, "/v1/", secretsEnginePath, "data/", sp])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

deleteSecretVersionsR
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
deleteSecretVersionsR VaultConnection{..} (SecretPath sp) vs =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "delete/", sp])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBody (RequestBodyLBS $ A.encode vs)

unDeleteSecretVersionsR
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
unDeleteSecretVersionsR VaultConnection{..} (SecretPath sp) vs =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "undelete/", sp])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBody (RequestBodyLBS $ A.encode vs)

destroySecretR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
destroySecretR VaultConnection{..} (SecretPath sp) =
  parseRequest (concat ["DELETE ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

destroySecretVersionsR
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
destroySecretVersionsR VaultConnection{..} (SecretPath sp) vs =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "destroy/", sp])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBody (RequestBodyLBS $ A.encode vs)

secretsListR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
secretsListR VaultConnection{..} (SecretPath sp) =
  if last sp /= '/'
    then return (Left "SecretPath must be a folder/")
    else parseRequest (concat ["LIST ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp])
         >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

readSecretMetadataR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
readSecretMetadataR VaultConnection{..} (SecretPath sp) =
  parseRequest (concat ["GET ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

updateSecretMetadataR
  :: VaultConnection
  -> SecretPath
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
updateSecretMetadataR VaultConnection{..} (SecretPath sp) mvs casr =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBodyJSON
        SecretSettings
          { max_versions = mvs
          , cas_required = casr
          }

-- Utils
{-
secret
  :: Either String A.Value
  -> IO (Either String SecretData)
secret (Left s) = return (Left s)
secret (Right v) =
 return $
   case v ^? key "data" . key "data" of
     Just o ->
       case A.fromJSON o of
         A.Success sd -> Right sd
         A.Error e    -> Left e
     Nothing -> Left "Not a secret data JSON object"
-}
toSecretData :: [(T.Text,T.Text)] -> SecretData
toSecretData l = SecretData (HM.fromList l)

toSecretVersions :: [Int] -> SecretVersions
toSecretVersions is = SecretVersions $ V.fromList $ A.toJSON <$> is

