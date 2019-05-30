{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Internal (

  runRequest,
  vaultHeaders,
  toJSONName

) where

import           Control.Monad.Catch
import qualified Data.ByteString           as B
import qualified Data.Aeson                as A
import           Network.HTTP.Client
import           Network.HTTP.Types.Header

runRequest :: Request
           -> Manager
           -> IO (Either String A.Value)
runRequest r m =
  try (httpLbs r m) >>= \t -> pure $
    case t of -- TODO -> Replace with either
      Right b ->
        Right $
          case A.decode (responseBody b) of
            Just v  -> v
            Nothing -> A.Null
      Left  e -> Left $ "error: " ++ show (e::SomeException)

vaultHeaders :: B.ByteString -- ^ Vault token
             -> [Header]
vaultHeaders vt =
  [ ("Content-Type", "application/json; charset=utf-8")
  , ("X-Vault-Token", vt)
  ]

toJSONName :: String -> String
toJSONName "secret_data"     = "data"
toJSONName "secret_metadata" = "metadata"
toJSONName "response_data"   = "data"
toJSONName s                 = s

