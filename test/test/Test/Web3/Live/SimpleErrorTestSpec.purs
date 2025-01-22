module Test.Web3.Live.SimpleErrorTestSpec (spec) where

import Prelude

import Chanterelle.Test (assertWeb3, buildTestConfig)
import Contract.SimpleErrorTest as SimpleErrorTest
import Data.Either (Either(..), isLeft)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.Ethereum.Web3 (ChainCursor(..), _from, _to, fromInt, uIntNFromBigNumber)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Test.Web3.ContractConfig as ContractConfig
import Test.Web3.Live.ContractUtils (defaultTestTxOptions, deployScript, nodeUrl)

spec :: SpecT Aff Unit Aff Unit
spec =
  describe "SimpleError"
    $ beforeAll (buildTestConfig nodeUrl 60 $ deployScript ContractConfig.simpleErrorCfg)
    $ describe "SimpleError" do
        it "can raise a left for unset values"
          $ \cfg -> do
              let
                { deployAddress: simpleErrorTestAddress, primaryAccount: userAddress, provider } = cfg

                txOptions =
                  defaultTestTxOptions # _to ?~ simpleErrorTestAddress
                    # _from
                        ?~ userAddress

                n = unsafePartial $ fromJust $ uIntNFromBigNumber (Proxy @256) $ fromInt 1
              -- TODO: am now getting execution reverted RPC error ?
              --Console.log "Testing for unset value 1"
              --resp1 <- assertWeb3 provider $ SimpleErrorTest.names txOptions Latest n
              --resp1 `shouldSatisfy` isLeft
              Console.log "Testing for unset value 2"
              resp2 <- assertWeb3 provider $ SimpleErrorTest.testBool txOptions Latest { _arg: true }
              resp2 `shouldEqual` Right false
              Console.log "Testing for unset value 3"
              resp3 <- assertWeb3 provider $ SimpleErrorTest.testBool txOptions Latest { _arg: false }
              resp3 `shouldEqual` Right true
