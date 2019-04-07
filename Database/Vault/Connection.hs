module Database.Vault.Connection where

import Database.Vault.KV.V2.Types
import System.Environment (lookupEnv)
-- import Data.Text as T
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as C

-- getVaultConfig :: IO (Either String VaultConfig)
getVaultConfig :: IO VaultConfig
getVaultConfig = do
  Just va <- lookupEnv "VAULT_ADDR"
  Just hm <- lookupEnv "HOME"
  vt      <- readFile (hm ++ "/.vault-token")
  return VaultConfig { vaultAddr = va, vaultToken = C.pack vt, secretsEnginePath = "secret/" }

getVaultConnection :: VaultConfig -> IO VaultConnection
getVaultConnection c = 
  newManager defaultManagerSettings >>= \m -> return VaultConnection { config = c, manager = m }

-- getSecretsList
