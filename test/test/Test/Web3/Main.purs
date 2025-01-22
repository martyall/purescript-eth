module Test.Web3.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Spec (parallel)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.Web3.Live.ComplexStorageSpec as ComplexStorageSpec
import Test.Web3.Live.FilterSpec as FilterSpec
import Test.Web3.Live.MockERC20Spec as MockERC20Spec
import Test.Web3.Live.MultifilterSpec as MultifilterSpec
import Test.Web3.Live.NestedTuplesSpec as NestedTuplesSpec
import Test.Web3.Live.PayableTestSpec as PayableTestSpec
import Test.Web3.Live.SimpleErrorTestSpec as SimpleErrorTestSpec
import Test.Web3.Live.SimpleStorageSpec as SimpleStorageSpec

main :: Effect Unit
main =
  launchAff_ do
    let
      cfg = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
    void $ join
      $ runSpecT cfg [ consoleReporter ] do
          parallel do
            -- all of these tests only have one `it` statement and
            -- are dealing with separate contracts so they can be run
            -- in parallel
            SimpleStorageSpec.spec
            NestedTuplesSpec.spec
            ComplexStorageSpec.spec
            MockERC20Spec.spec
            SimpleErrorTestSpec.spec
          MultifilterSpec.spec
          -- FilterSpec.spec
          -- payable spec can't be run in parallel :/
          PayableTestSpec.spec
