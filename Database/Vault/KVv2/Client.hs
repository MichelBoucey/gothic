{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

--------------------------------------------------------------------------------------------------
-- | See https://www.vaultproject.io/api/secret/kv/kv-v2.html for HashiCorp Vault KVv2 API details
--------------------------------------------------------------------------------------------------

module Database.Vault.KVv2.Client (

    VaultConnection,

    -- * Connect & configure Vault KVv2 Engine
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

    currentSecretVersion,
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
--
-- >λ: vaultConnect (Just "https://vault.local.lan:8200/") "/secret" Nothing False
--
vaultConnect
  :: Maybe String                       -- ^ Use 'Just' this string as Vault address or get it from variable environment VAULT_ADDR
  -> String                             -- ^ KV engine path
  -> Maybe VaultToken                   -- ^ Use 'Just' this 'VaultToken' or get it from $HOME/.vaut-token
  -> Bool                               -- ^ Disable certificate validation
  -> IO (Either String VaultConnection)
vaultConnect mva kvep mvt dcv = do
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
                 let fp = M.fromJust hm ++ "/.vault-token"
                 if M.isJust va
                   then do
                     fe <- fileExist fp
                     if fe 
                       then Right <$> B.readFile fp
                       else return (Left $ "No Vault token file found at " ++ fp)
                   else return (Left "Variable environment VAULT_ADDR not set")
               else return (Left "Variable environment HOME not set")
  pure $
    (\vt ->
      VaultConnection
        { vaultAddr    = M.fromJust va
        , vaultToken   = vt
        , kvEnginePath = kvep
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
  configR (concat ["POST ", show vc, "/config"]) vc

-- | Override default secret settings for the given secret.
secretConfig
  :: VaultConnection
  -> SecretPath
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
secretConfig vc@VaultConnection{..} SecretPath{..} =
  configR (concat ["POST ", show vc, "/metadata/", path ]) vc

-- | Get a secret from Vault. Give 'Just' the 'SecretVersion'
-- to retrieve or 'Nothing' to get the current one.
--
-- >λ>getSecret conn (SecretPath "MySecret") Nothing
-- >Right (SecretData (fromList [("my","password")]))
--
getSecret
  :: VaultConnection
  -> SecretPath
  -> Maybe SecretVersion
  -> IO (Either String SecretData)
getSecret vc sp msv = do
  -- secret <$> getSecretR vc sp msv
  r <- getSecretR vc sp msv
  return (r >>= secret)

-- | Put 'SecretData' into Vault at the given location.
putSecret
  :: VaultConnection
  -> CheckAndSet                      -- ^ 'WriteAllowed', 'CreateOnly' or 'CurrentVersion'
  -> SecretPath
  -> SecretData                       -- ^ Data to put at 'SecretPath' location
  -> IO (Either String SecretVersion)
putSecret vc cas sp sd = do
  r <- putSecretR vc cas sp sd
  return (r >>= version)

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
  maybeError <$> secretVersionsR (concat ["POST ", show vc, "/delete/", path]) vc svs

unDeleteSecretVersions
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Maybe Error)
unDeleteSecretVersions vc@VaultConnection{..} SecretPath{..} svs =
  maybeError <$> secretVersionsR (concat ["POST ", show vc, "/undelete/", path]) vc svs

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
  secretVersionsR (concat ["POST ", show vc, "/destroy/", path]) vc

-- | Get list of secrets and folders at the given location.
secretsList
  :: VaultConnection
  -> SecretPath
  -> IO (Either String [VaultKey])
secretsList vc sp = do
  r <- secretsListR vc sp
  return (r >>= list)

-- | Retrieve versions history of the given secret.
--
-- >λ: readSecretMetadata conn (SecretPath "MySecret") 
-- >Right (SecretMetadata (fromList [(SecretVersion 1,Metadata {destroyed = True, deletion_time = "", created_time = "2019-05-30T13:22:58.416399224Z"}),(SecretVersion 2,Metadata {destroyed = True, deletion_time = "2019-06-29T15:28:46.145302138Z"})]))
--
readSecretMetadata
  :: VaultConnection
  -> SecretPath
  -> IO (Either String SecretMetadata)
readSecretMetadata vc sp = do
  sm <- readSecretMetadataR vc sp
  return (sm >>= metadata)

-- | Get version number of the current given secret.
currentSecretVersion
  :: VaultConnection
  -> SecretPath
  -> IO (Either String SecretVersion)
currentSecretVersion vc sp = do
  r <- readSecretMetadataR vc sp
  return (r >>= current)

-- Utils

toSecretData
  :: [(Text,Text)]
  -> SecretData
toSecretData = SecretData . fromList

fromSecretData
  :: SecretData
  -> [(Text,Text)]
fromSecretData (SecretData sd) = toList sd

toSecretVersions
  :: [Int]
  -> SecretVersions
toSecretVersions is =
  SecretVersions (SecretVersion <$> is)

