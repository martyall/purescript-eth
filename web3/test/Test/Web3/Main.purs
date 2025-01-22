module Test.Web3.Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.Ethereum.Web3.Types.Provider (httpProvider)
import Test.Spec (Spec, SpecT, mapSpecTree)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.Web3.Encoding.ContainersSpec as EncodingContainersSpec
import Test.Web3.Encoding.DataSpec as EncodingDataSpec
import Test.Web3.Encoding.GenericSpec as EncodingGenericSpec
import Test.Web3.Encoding.SimpleSpec as EncodingSimpleSpec
import Test.Web3.Live.RPCSpec as RPCSpec
import Test.Web3.Types.EtherUnitSpec as EtherUnitSpec
import Test.Web3.Types.VectorSpec as VectorSpec

-- import Test.Web3.Types.EtherUnitSpec as EtherUnitSpec

main :: Effect Unit
main =
  launchAff_
    do
      Console.log "Running tests..."
      let
        cfg = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
      p <- liftEffect $ httpProvider "http://localhost:8545"
      void $ join $ runSpecT cfg [ consoleReporter ] do
        hoist do
          EncodingDataSpec.spec
          EncodingContainersSpec.spec
          EncodingSimpleSpec.spec
          EncodingGenericSpec.spec
          EtherUnitSpec.spec
          VectorSpec.spec
        RPCSpec.spec p
  where
  hoist :: Spec ~> SpecT Aff Unit Aff
  hoist = mapSpecTree (pure <<< un Identity) identity
