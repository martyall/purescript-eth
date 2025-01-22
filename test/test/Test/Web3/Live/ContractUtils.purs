module Test.Web3.Live.ContractUtils where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Test (assertWeb3)
import Chanterelle.Types.Deploy (ContractConfig, DeployConfig(..), DeployM)
import Control.Monad.Gen (chooseInt, frequency, suchThat)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array (foldMap)
import Data.Array.NonEmpty as NAE
import Data.Enum (toEnumWithDefaults)
import Data.Int (toNumber)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty(..))
import Data.String (CodePoint, fromCodePointArray)
import Data.Traversable (intercalate)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as C
import Network.Ethereum.Core.BigNumber (decimal, fromStringAs)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber, HexString, Provider, TransactionOptions, _from, _gas, defaultTransactionOptions, mkHexString)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf)
import Test.Spec (ComputationType(..), SpecT, hoistSpec)

type Logger m = String -> m Unit

go :: SpecT (ReaderT (Logger Aff) Aff) Unit Aff ~> SpecT Aff Unit Aff
go =
  hoistSpec identity \cType m ->
    let
      prefix = case cType of
        CleanUpWithContext n -> intercalate " > " n <> " (afterAll) "
        TestWithName n -> intercalate " > " $ NAE.toArray n
    in
      runReaderT m \logMsg -> C.log $ prefix <> "| " <> logMsg

hangOutTillBlock
  :: forall m
   . MonadAff m
  => Provider
  -> Logger m
  -> BlockNumber
  -> m Unit
hangOutTillBlock provider logger bn = do
  bn' <- liftAff $ assertWeb3 provider Api.eth_blockNumber
  logger $ "Current block number : " <> show bn'
  when (bn' < bn) do
    liftAff $ delay (Milliseconds 1000.0)
    hangOutTillBlock provider logger bn

awaitNextBlock
  :: forall m
   . MonadAff m
  => Provider
  -> Logger m
  -> m Unit
awaitNextBlock provider logger = do
  n <- liftAff $ assertWeb3 provider Api.eth_blockNumber
  let
    next = wrap $ one + unwrap n
  logger $ "Awaiting block number " <> show next
  hangOutTillBlock provider logger next

mkHexString'
  :: String
  -> HexString
mkHexString' hx = unsafePartial fromJust $ mkHexString hx

defaultTestTxOptions :: TransactionOptions NoPay
defaultTestTxOptions = defaultTransactionOptions # _gas ?~ bigGasLimit

bigGasLimit :: BigNumber
bigGasLimit = unsafePartial fromJust $ fromStringAs decimal "4712388"

deployScript :: forall a. ContractConfig a -> DeployM { deployAddress :: Address, primaryAccount :: Address }
deployScript contractConfig = do
  DeployConfig { primaryAccount } <- ask
  let txOpts = defaultTestTxOptions # _from ?~ primaryAccount
  { deployAddress } <- deployContract txOpts contractConfig
  pure { deployAddress, primaryAccount }

nodeUrl :: String
nodeUrl = "http://localhost:8545"

newtype BMPString = BMPString String

derive newtype instance Eq BMPString
derive newtype instance Show BMPString

derive instance Newtype BMPString _

instance Arbitrary UnicodeChar where
  arbitrary = frequency $ NonEmpty (Tuple (1.0 - p) normalGen) [ Tuple p surrogatesGen ]

    where
    hiLB = 0xD800
    hiUB = 0xDBFF
    loLB = 0xDC00
    loUB = 0xDFFF
    maxCP = 65535
    toCP = toEnumWithDefaults bottom top
    -- must have a high surrogate followed by a low surrogate
    surrogatesGen = Surrogates <$> (toCP <$> chooseInt hiLB hiUB) <*> (toCP <$> chooseInt loLB loUB)
    normalGen = Normal <<< toCP <$> do
      chooseInt 0 maxCP `suchThat` \n ->
        (n < hiLB || n > hiUB) && (n < loLB || n > loUB)
    -- probability that you pick a surrogate from all possible codepoints
    p = toNumber ((hiUB - hiLB + 1) + (loUB - loLB + 1)) / toNumber (maxCP + 1)

data UnicodeChar = Normal CodePoint | Surrogates CodePoint CodePoint

instance Arbitrary BMPString where
  arbitrary = BMPString <$> do
    ucs <- arrayOf arbitrary
    pure $ fromCodePointArray $ foldMap f ucs
    where
    f uc = case uc of
      Normal a -> [ a ]
      Surrogates a b -> [ a, b ]
