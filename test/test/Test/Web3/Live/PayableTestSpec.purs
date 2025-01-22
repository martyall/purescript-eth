module Test.Web3.Live.PayableTestSpec (spec) where

import Prelude

import Chanterelle.Test (assertWeb3, buildTestConfig, takeEvent)
import Contract.PayableTest as PayableTest
import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Web3 (Ether, Shannon, Value, _from, _to, _value, convert, mkValue, unUIntN)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Test.Web3.ContractConfig as ContractConfig
import Test.Web3.Live.ContractUtils (defaultTestTxOptions, deployScript, nodeUrl)

spec :: SpecT Aff Unit Aff Unit
spec =
  describe "PayableTest"
    $ beforeAll (buildTestConfig nodeUrl 60 $ deployScript ContractConfig.payableCfg)
    $ describe "PayableTest" do
        it "can send the right amount of Ether"
          $ \cfg -> do
              let
                { deployAddress: payableTestAddress, primaryAccount: userAddress, provider } = cfg

                txOptions =
                  defaultTestTxOptions # _to ?~ payableTestAddress
                    # _value
                        ?~ convert (mkValue one :: Value Ether)
                    # _from
                        ?~ userAddress

                etherAction = PayableTest.seeContent txOptions
              Tuple _ (PayableTest.Content c) <- liftAff
                $ assertWeb3 provider
                $ takeEvent (Proxy :: Proxy PayableTest.Content) payableTestAddress etherAction
              unUIntN c._paidContent `shouldEqual` one
        it "can send the right amount of Shannon"
          $ \cfg -> do
              let
                { deployAddress: payableTestAddress, primaryAccount: userAddress, provider } = cfg

                txOptions =
                  defaultTestTxOptions # _to ?~ payableTestAddress
                    # _value
                        ?~ convert (mkValue one :: Value Shannon)
                    # _from
                        ?~ userAddress

                etherAction = PayableTest.seeContent txOptions
              Tuple _ (PayableTest.Content c) <-
                assertWeb3 provider
                  $ takeEvent (Proxy :: Proxy PayableTest.Content) payableTestAddress etherAction
              unUIntN c._paidContent `shouldEqual` zero
