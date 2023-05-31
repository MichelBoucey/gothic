{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Internal where

import           Control.Lens
import           Control.Monad.Catch
import qualified Data.Aeson                as A
import           Data.Aeson.Key            (fromText)
import           Data.Aeson.Lens
import qualified Data.ByteString           as B
import           Data.List                 as L
import           Data.List.NonEmpty        as N
import qualified Data.Maybe                as M
import           Data.Scientific
import           Data.Text                 as T
import qualified Data.Vector               as V
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

fromVaultResponse
  :: T.Text
  -> (A.Value -> Either String a)
  -> A.Value
  -> Either String a
fromVaultResponse k f v =
  case v ^? key "data" . key (fromText k) of
    Just o@(A.Object _) -> f o
    Just n@(A.Number _) -> f n
    Just a@(A.Array  _) -> f a
    Just _              -> Left "Unexpected JSON type"
    Nothing             -> Left (jsonErrors v)

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
            then "Undetermined error"
            else
              L.intercalate
                ", "
                (toString <$> V.toList a) ++ "."
        _anyOther -> "Expected JSON array"
    Nothing -> expectedJSONField "errors"

toString :: A.Value -> String
toString (A.String s) = T.unpack s
toString _            = fail "Expected JSON String"

expectedJSONField :: String -> String
expectedJSONField = (++) "Expected JSON field not found: "

unexpectedJSONType :: Either String b
unexpectedJSONType = Left "Unexpected JSON type"

toInt :: Scientific -> Int
toInt = M.fromJust . toBoundedInteger

hasTrailingSlash :: String -> Bool
hasTrailingSlash s = N.last (N.fromList s) == '/'

removeTrailingSlash :: String -> String
removeTrailingSlash s =
  if hasTrailingSlash s
    then N.init (N.fromList s)
    else s

hasLeadingSlash :: String -> Bool
-- hasLeadingSlash s = N.head (N.fromList s) == '/'
hasLeadingSlash s = N.head (N.fromList s) == '/'

removeLeadingSlash :: String -> String
removeLeadingSlash s =
  if hasLeadingSlash s
    then N.tail (N.fromList s)
    else s

