module Database.Vault.KVv2.Client.Connection where

import qualified Data.ByteString.Char8 as C
import           Network.Connection 
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment (lookupEnv)

import Database.Vault.KVv2.Client.Types

getVaultConfig :: IO VaultConfig
getVaultConfig = do
  Just va <- lookupEnv "VAULT_ADDR"
  Just hm <- lookupEnv "HOME"
  vt      <- readFile (hm ++ "/.vault-token")
  return VaultConfig { vaultAddr = va, vaultToken = C.pack vt, secretsEnginePath = "secret/" }

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
