{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Internal where

import           Control.Monad.Catch
import           Control.Lens
import qualified Data.ByteString           as B
import qualified Data.Aeson                as A
import           Data.Aeson.Lens
import qualified Data.Maybe                as M
import           Data.Scientific
import           Data.List                 as L
import           Data.Text                 as T
import           Network.HTTP.Client
import           Network.HTTP.Types.Header
import qualified Data.Vector               as V

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

jsonErrors :: A.Value -> String
jsonErrors v =
  case v ^? key "errors" of
    Just ja ->
      case ja of
        A.Array a ->
          if a == mempty
            then "Undetermined error."
            else
              L.intercalate
                ", "
                (toString <$> V.toList a) ++ "."
        _         -> "Unexpected JSON type"
    Nothing -> expectedJSONField "errors"

toString :: A.Value -> String
toString (A.String s) = T.unpack s
toString _            = fail "Expecting JSON type String only"

expectedJSONField :: String -> String
expectedJSONField f = "Expected JSON field not found: " ++ f

unexpectedJSONType :: Either String b
unexpectedJSONType = Left "Unexpected JSON type"

toInt :: Scientific -> Int
toInt = M.fromJust . toBoundedInteger

hasTrailingSlash :: String -> Bool
hasTrailingSlash s = s /= mempty && L.last s == '/'

removeTrailingSlash :: String -> String
removeTrailingSlash s =
  if hasTrailingSlash s
    then L.init s
    else s

hasLeadingSlash :: String -> Bool
hasLeadingSlash s = s /= mempty && L.head s == '/'

removeLeadingSlash :: String -> String
removeLeadingSlash s =
  if hasLeadingSlash s
    then L.tail s
    else s
