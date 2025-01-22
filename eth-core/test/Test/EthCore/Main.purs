module Test.EthCore.Main where

import Prelude

import Test.EthCore.Spec.BigNumber (bigNumberSpec)
import Test.EthCore.Spec.Hex (hexSpec)
import Test.EthCore.Spec.Keccak256 (keccak256Spec)
import Test.EthCore.Spec.RLP (rlpSpec)
import Test.EthCore.Spec.Signatures (signatureSpec)
import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ]
  $ do
      keccak256Spec
      hexSpec
      bigNumberSpec
      rlpSpec
      signatureSpec
