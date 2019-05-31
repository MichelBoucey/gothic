{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Internal (

  runRequest,
  vaultHeaders,
  toJSONName

) where

import           Control.Monad.Catch
import qualified Data.ByteString           as B
import qualified Data.Aeson                as A
import qualified Data.Maybe                as M
import           Network.HTTP.Client
import           Network.HTTP.Types.Header

runRequest :: Manager
           -> Request
           -> IO (Either String A.Value)
runRequest m r =
  try (httpLbs r m) >>= \t -> pure $
    case t of
      Right b ->
        Right (M.fromMaybe A.Null $ A.decode $ responseBody b)
      Left  e -> Left $ "error: " ++ show (e::SomeException)

vaultHeaders :: B.ByteString -- ^ Vault token
             -> [(HeaderName, B.ByteString)]
vaultHeaders vt =
  [ ("Content-Type", "application/json; charset=utf-8")
  , ("X-Vault-Token", vt)
  ]

toJSONName :: String -> String
toJSONName "secret_data"     = "data"
toJSONName "secret_metadata" = "metadata"
toJSONName "response_data"   = "data"
toJSONName s                 = s

