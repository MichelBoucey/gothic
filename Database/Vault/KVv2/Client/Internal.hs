{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Internal where

import           Control.Monad.Catch
import qualified Data.ByteString           as B
import qualified Data.Aeson                as A
import qualified Data.Maybe                as M
import           Data.Scientific
import           Network.HTTP.Client
import           Network.HTTP.Types.Header

runRequest
  :: Manager
  -> Request
  -> IO (Either String A.Value)
runRequest m r =
  esv <$> try (httpLbs r m)
  where
  esv t =
    case t of
      Right b ->
        pure (M.fromMaybe A.Null $ A.decode $ responseBody b)
      Left  e -> Left $ show (e::SomeException)

vaultHeaders
  :: B.ByteString -- ^ Vault token
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

toInt :: Scientific -> Int
toInt = M.fromJust . toBoundedInteger

hasTrailingSlash :: String -> Bool
hasTrailingSlash s = s /= mempty && last s == '/'

removeTrailingSlash :: String -> String
removeTrailingSlash s =
  if hasTrailingSlash s
    then init s
    else s

hasLeadingSlash :: String -> Bool
hasLeadingSlash s = s /= mempty && head s == '/'

removeLeadingSlash :: String -> String
removeLeadingSlash s =
  if hasLeadingSlash s
    then tail s
    else s
