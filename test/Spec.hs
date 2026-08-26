--
-- To be used with a docker container launch with:
-- docker run --cap-add=IPC_LOCK -e 'VAULT_DEV_ROOT_TOKEN_ID=A_SECRET_TOKEN' -d --name=dev-vault -p 8200:8200 hashicorp/vault
--
module Main (main) where

import           Test.Hspec

import qualified ClientSpec
import qualified UtilsSpec

main :: IO ()
main = hspec $ do
  UtilsSpec.spec
  ClientSpec.spec
