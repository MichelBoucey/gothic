module Database.Vault.KVv2.Client.Connection where

import qualified Data.ByteString.Char8   as C
import           Network.Connection 
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment      (lookupEnv)
import           System.Posix.Files      (fileExist)
import           Data.ByteString         as B

import           Database.Vault.KVv2.Client.Types

getVaultConfig :: String
               -> Maybe VaultToken
               -> IO (Either String VaultConfig)
getVaultConfig sep mvt = do
  Just va <- lookupEnv "VAULT_ADDR"
  evt <- case mvt of
             Just t  -> return $ Right (C.pack t)
             Nothing -> do
               Just hm <- lookupEnv "HOME" -- TODO
               let pf = hm ++ "/.vault-token"
               fe <- fileExist pf
               if fe
                 then Right <$> B.readFile pf
                 else return $ Left ("No Vault token file found at " ++ pf)
  return $ case evt of
    Right vt -> Right $ VaultConfig { vaultAddr = va, vaultToken = vt, secretsEnginePath = sep }
    Left s   -> Left s

getVaultConnection :: VaultConfig -> IO VaultConnection
getVaultConnection c = 
  newTlsManagerWith tlsDisableCertValidation >>= \m -> return VaultConnection { config = c, manager = m }

tlsDisableCertValidation :: ManagerSettings
tlsDisableCertValidation =
  mkManagerSettings
    TLSSettingsSimple
      { settingDisableCertificateValidation = True
      , settingDisableSession               = False
      , settingUseServerName                = True }
    Nothing
