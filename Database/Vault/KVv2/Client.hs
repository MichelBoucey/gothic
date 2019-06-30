{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------------------------
-- | For HashiCorp Vault KVv2 API details: https://www.vaultproject.io/api/secret/kv/kv-v2.html
-----------------------------------------------------------------------------------------------

module Database.Vault.KVv2.Client (

    -- * Connect and configure Vault KV v2 Engine
    vaultConnect,
    kvEngineConfig,
    secretConfig,
  
    -- * Basic operations

    putSecret,
    getSecret,
  
    -- * Soft secret deletion
    deleteSecret,
    deleteSecretVersions,
    unDeleteSecretVersions,
  
    -- * Permanent secret deletion
    destroySecret,
    destroySecretVersions,
  
    -- * Get informations

    secretCurrentVersion,
    readSecretMetadata,
    secretsList,

    -- * Utils
    toSecretData,
    fromSecretData,
    toSecretVersions,

  ) where

import qualified Data.Aeson                          as A
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as C
import           Data.HashMap.Strict
import qualified Data.Maybe                          as M
import           Data.Text                           hiding (concat)
import           Network.Connection 
import           Network.HTTP.Client.TLS
import           System.Environment                  (lookupEnv)
import           System.Posix.Files                  (fileExist)

import           Database.Vault.KVv2.Client.Types
import           Database.Vault.KVv2.Client.Lens
import           Database.Vault.KVv2.Client.Requests

-- | Get a 'VaultConnection', or an error message.
vaultConnect
  :: Maybe String                       -- ^ Use 'Just' this string as Vault address or get it from variable environment VAULT_ADDR
  -> String                             -- ^ KV engine path
  -> Maybe VaultToken                   -- ^ Use 'Just' this Vault token or get it from $HOME/.vaut-token
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
        { vaultAddr    = M.fromJust va
        , vaultToken   = vt
        , kvEnginePath = sep
        , manager      = nm
        }
    ) <$> evt

-- | Set default secret settings for the KVv2 engine.
kvEngineConfig
  :: VaultConnection
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
kvEngineConfig vc@VaultConnection{..} =
  configR (concat ["POST ", vaultAddr, "/v1/", kvEnginePath, "/config"]) vc

-- | Override default secret settings for the given secret.
secretConfig
  :: VaultConnection
  -> SecretPath
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
secretConfig vc@VaultConnection{..} SecretPath{..} =
  configR (concat ["POST ", vaultAddr, "/v1/", kvEnginePath, "/metadata/", path ]) vc

-- | Get a secret from Vault. Give 'Just' the 'SecretVersion'
-- to retrieve or 'Nothing' to get the current one.
--
-- >λ>getSecret conn (SecretPath "MySecret") Nothing
-- >Right (SecretData (fromList [("Top","secret!")]))
--
getSecret
  :: VaultConnection
  -> SecretPath
  -> Maybe SecretVersion
  -> IO (Either String SecretData)
getSecret vc sp msv =
  secret <$> getSecretR vc sp msv

-- | Put 'SecretData' into Vault at the given location.
putSecret
  :: VaultConnection
  -> CheckAndSet                      -- ^ 'WriteAllowed', 'CreateOnly' or 'CurrentVersion'
  -> SecretPath
  -> SecretData                       -- ^ Data to put at 'SecretPath' location
  -> IO (Either String SecretVersion)
putSecret vc cas sp sd =
  version <$> putSecretR vc cas sp sd

deleteSecret
  :: VaultConnection
  -> SecretPath
  -> IO (Maybe Error)
deleteSecret vc sp =
  maybeError <$> deleteSecretR vc sp

deleteSecretVersions
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Maybe Error)
deleteSecretVersions vc@VaultConnection{..} SecretPath{..} svs =
  maybeError <$> secretVersionsR (concat ["POST ", vaultAddr, "/v1/", kvEnginePath, "/delete/", path]) vc svs

unDeleteSecretVersions
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Maybe Error)
unDeleteSecretVersions vc@VaultConnection{..} SecretPath{..} svs =
  maybeError <$> secretVersionsR (concat ["POST ", vaultAddr, "/v1/", kvEnginePath, "/undelete/", path]) vc svs

-- | Permanently delete a secret, i.e. all its versions and metadata.
destroySecret
  :: VaultConnection
  -> SecretPath
  -> IO (Maybe Error)
destroySecret vc sp =
  maybeError <$> destroySecretR vc sp

destroySecretVersions
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
destroySecretVersions vc@VaultConnection{..} SecretPath{..} =
  secretVersionsR (concat ["POST ", vaultAddr, "/v1/", kvEnginePath, "/destroy/", path]) vc

-- | Get list of secrets and folders at the given location.
secretsList
  :: VaultConnection
  -> SecretPath
  -> IO (Either String [VaultKey])
secretsList vc sp =
  list <$> secretsListR vc sp

-- | Retrieve versions history of the given secret.
--
-- >λ: readSecretMetadata conn (SecretPath "MySecret") 
-- >Right (SecretMetadata (fromList [(SecretVersion 1,Metadata {destroyed = True, deletion_time = "", created_time = "2019-05-30T13:22:58.416399224Z"}),(SecretVersion 2,Metadata {destroyed = True, deletion_time = "2019-06-29T15:28:46.145302138Z"})]))
--
readSecretMetadata
  :: VaultConnection
  -> SecretPath
  -> IO (Either String SecretMetadata)
readSecretMetadata vc sp =
  metadata <$> readSecretMetadataR vc sp

-- | Get version number of the current given secret.
secretCurrentVersion
  :: VaultConnection
  -> SecretPath
  -> IO (Either String SecretVersion)
secretCurrentVersion vc sp =
  current <$> readSecretMetadataR vc sp

-- Utils

toSecretData
  :: [(Text,Text)]
  -> SecretData
toSecretData = SecretData . fromList

fromSecretData
  :: SecretData
  -> [(Text,Text)]
fromSecretData = undefined -- TODO

toSecretVersions
  :: [Int]
  -> SecretVersions
toSecretVersions is =
  SecretVersions (SecretVersion <$> is)

