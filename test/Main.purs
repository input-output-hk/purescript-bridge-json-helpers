module Test.Main where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Argonaut.Decode.Aeson (Decoder, (</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson (Encoder, (>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Op (Op(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck (class Arbitrary, arbitrary, (===))
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Test.QuickCheck.Gen (Gen, enum)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

newtype Map k v = Map (Map.Map k v)

derive instance genericMap :: Generic (Map k v) _

derive instance newtypeMap :: Newtype (Map k v) _

derive instance eqMap :: (Eq k, Eq v) => Eq (Map k v)

instance showMap :: (Show k, Show v) => Show (Map k v) where
  show = genericShow

instance arbitraryMap ::
  ( Ord k
  , Arbitrary k
  , Arbitrary v
  ) =>
  Arbitrary (Map k v) where
  arbitrary =
    Map <<< Map.fromFoldable <$> (arbitrary :: Gen (Array (Tuple k v)))

data Product = P Int String

derive instance genericProduct :: Generic Product _

derive instance eqProduct :: Eq Product

instance showProduct :: Show Product where
  show = genericShow

instance arbitraryProduct :: Arbitrary Product where
  arbitrary = genericArbitrary

pToTuple :: Product -> Tuple Int String
pToTuple (P n s) = n /\ s

data SumType = Foo String | Bar Int

derive instance genericSumType :: Generic SumType _

derive instance eqSumType :: Eq SumType

instance showSumType :: Show SumType where
  show = genericShow

instance arbitrarySumType :: Arbitrary SumType where
  arbitrary = genericArbitrary

data Enum = A | B | C

derive instance eqEnum :: Eq Enum

derive instance ordEnum :: Ord Enum

instance enumEnum :: Enum Enum where
  succ = genericSucc
  pred = genericPred

instance boundedEnum :: Bounded Enum where
  bottom = genericBottom
  top = genericTop

instance boundedEnumEnum :: BoundedEnum Enum where
  cardinality = Cardinality 3
  fromEnum A = 0
  fromEnum B = 1
  fromEnum C = 2
  toEnum 0 = Just A
  toEnum 1 = Just B
  toEnum 2 = Just C
  toEnum _ = Nothing

derive instance genericEnum :: Generic Enum _

instance showEnum :: Show Enum where
  show = genericShow

instance arbitraryEnum :: Arbitrary Enum where
  arbitrary = enum

intDecoder :: Decoder Int
intDecoder = D.value

intEncoder :: Encoder Int
intEncoder = E.value

stringDecoder :: Decoder String
stringDecoder = D.value

stringEncoder :: Encoder String
stringEncoder = E.value

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "either" $
    roundtripSpec
      (D.either stringDecoder intDecoder)
      (E.either stringEncoder intEncoder)
  describe "enum" $ roundtripSpec (D.enum :: Decoder Enum) E.enum
  describe "maybe" $ roundtripSpec (D.maybe intDecoder) (E.maybe intEncoder)
  describe "record" $
    roundtripSpec
      (D.record "Test" { foo: D.maybe intDecoder, bar: stringDecoder })
      (E.record { foo: E.maybe intEncoder, bar: stringEncoder })
  describe "sumType" $
    roundtripSpec
      ( D.sumType "SumType"
          $ Map.fromFoldable
              [ "Foo" /\ D.content (Foo <$> stringDecoder)
              , "Bar" /\ D.content (Bar <$> intDecoder)
              ]
      )
      ( Op case _ of
          Foo a -> E.encodeTagged "Foo" a stringEncoder
          Bar a -> E.encodeTagged "Bar" a intEncoder
      )
  describe "tuple" $
    roundtripSpec
      (D.tuple $ intDecoder </\> stringDecoder </\> D.maybe intDecoder)
      (E.tuple $ intEncoder >/\< stringEncoder >/\< E.maybe intEncoder)
  describe "tupleApply" $
    roundtripSpec
      (D.tuple $ P </$\> intDecoder </*\> stringDecoder)
      (E.tuple $ pToTuple >$< intEncoder >/\< stringEncoder)
  describe "unit" $ roundtripSpec D.unit E.unit
  describe "dictionary" $
    roundtripSpec
      (Map <$> D.dictionary Just intDecoder)
      (un Map >$< E.dictionary identity intEncoder)

roundtripSpec
  :: forall a
   . Arbitrary a
  => Eq a
  => Show a
  => Decoder a
  -> Encoder a
  -> Spec Unit
roundtripSpec decoder encoder =
  it "obeys the roundtrip law"
    $ quickCheck \a -> runReaderT decoder (un Op encoder a) === Right a
