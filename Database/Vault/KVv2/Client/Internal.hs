{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Internal (

  runRequest,
  vaultHeaders

) where

import           Control.Monad.Catch
import qualified Data.ByteString           as B
import qualified Data.Aeson                as A
import           Network.HTTP.Client
import           Network.HTTP.Types.Header

runRequest :: Request -> Manager -> IO (Either String A.Value)
runRequest r m = do
  t <- try (httpLbs r m)
  return $ case t of
    Right b -> case A.decode (responseBody b) of
                 Just v  -> Right v
                 Nothing -> Left "Something went wrong..."
    Left  e -> Left $ "error" ++ show (e::SomeException)

vaultHeaders :: B.ByteString -> [Header]
vaultHeaders vt =
  [ ("Content-Type", "application/json; charset=utf-8")
  , ("X-Vault-Token", vt) ]
