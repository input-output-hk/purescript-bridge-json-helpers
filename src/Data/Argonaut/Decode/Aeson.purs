module Data.Argonaut.Decode.Aeson
  ( class RowListDecoder
  , class ToTupleDecoder
  , Decoder
  , content
  , either
  , enum
  , decode
  , maybe
  , null
  , record
  , rowListDecoder
  , sumType
  , toTupleDecoder
  , tuple
  , tupleApply
  , tupleConjoin
  , tupleMap
  , unit
  , value
  , (</\>)
  , (</*\>)
  , (</$\>)
  ) where

import Prelude hiding (unit)

import Control.Alt ((<|>))
import Control.Monad.RWS (RWSResult(..), RWST(..), evalRWST)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut.Aeson (contentsProp, leftProp, maybeToEither, rightProp, tagProp, unconsRecord)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Decode.Decoders (decodeArray, decodeJArray, decodeJObject, decodeNull, decodeString)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Array (find, index)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Enum (class Enum, upFromIncluding)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object)
import Foreign.Object as Obj
import Prelude (unit) as P
import Prim.Row as R
import Prim.RowList (class RowToList, Cons, Nil)
import Record as Rec
import Type.Prelude (Proxy(..))

type Decoder = ReaderT Json (Either JsonDecodeError)
type JPropDecoder = ReaderT (Object Json) (Either JsonDecodeError)
type TupleDecoder = RWST (Array Json) Unit Int (Either JsonDecodeError)

class ToTupleDecoder f where
  toTupleDecoder :: forall a. f a -> TupleDecoder a

instance toTupleDecoderDecoder ::
  ToTupleDecoder (ReaderT Json (Either JsonDecodeError)) where
  toTupleDecoder decoder =
    RWST \arr i ->
      RWSResult
        (i + 1)
        <$>
          ( lmap (AtIndex i)
              $ runReaderT decoder
                  =<< maybeToEither MissingValue (index arr i)
          )
        <*> pure P.unit

instance toTupleDecoderTupleDecoder ::
  ToTupleDecoder (RWST (Array Json) Unit Int (Either JsonDecodeError)) where
  toTupleDecoder = identity

class RowListDecoder :: forall k. k -> Row Type -> Row Type -> Constraint
class RowListDecoder rl ri ro | rl -> ri ro where
  rowListDecoder :: Proxy rl -> Record ri -> JPropDecoder (Record ro)

instance rowListDecoderNil :: RowListDecoder Nil () () where
  rowListDecoder _ _ = pure {}

instance rowListDecoderDecoderCons ::
  ( IsSymbol prop
  , R.Lacks prop tailDecoders
  , R.Cons prop (Decoder a) tailDecoders decoders
  , R.Lacks prop tailValues
  , R.Cons prop a tailValues values
  , RowListDecoder tail tailDecoders tailValues
  ) =>
  RowListDecoder (Cons prop (Decoder a) tail) decoders values where
  rowListDecoder _ decoders =
    let
      Tuple valueDecoder tailDecoders = unconsRecord (Proxy :: _ prop) decoders
    in
      Rec.insert (Proxy :: _ prop)
        <$> propDecoder (Proxy :: _ prop) valueDecoder
        <*> rowListDecoder (Proxy :: _ tail) tailDecoders

propDecoder
  :: forall p a
   . IsSymbol p
  => Proxy p
  -> Decoder a
  -> JPropDecoder a
propDecoder p decoder =
  let
    key = reflectSymbol p
  in
    ReaderT $
      lmap (AtKey key)
        <<< (runReaderT decoder)
        <=< maybeToEither MissingValue
          <<< Obj.lookup key

value :: forall a. DecodeJson a => Decoder a
value = ReaderT $ decodeJson

maybe :: forall a. Decoder a -> Decoder (Maybe a)
maybe decoder =
  Nothing <$ ReaderT decodeNull <|> Just <$> decoder

either :: forall a b. Decoder a -> Decoder b -> Decoder (Either a b)
either decoderA decoderB = ReaderT $ decodeJObject >=>
  decodeEitherObj
  where
  decodeEitherObj obj =
    Left <$> decodeLeft obj <|> Right <$> decodeRight obj
  decodeLeft obj = obj .: leftProp >>= runReaderT decoderA
  decodeRight obj = obj .: rightProp >>= runReaderT decoderB

enum :: forall a. Enum a => Bounded a => Show a => Decoder a
enum = ReaderT \json -> do
  v <- decodeString json
  maybeToEither (UnexpectedValue json)
    $ find ((v == _) <<< show)
    $ upFromIncluding bottom

sumType :: forall a. String -> Map String (JPropDecoder a) -> Decoder a
sumType name decoders = ReaderT \json -> lmap (Named name) do
  obj <- decodeJObject json
  tag <- obj .: tagProp
  decoders
    # Map.lookup tag
    # map (flip runReaderT obj)
    # fromMaybe (Left $ AtKey tagProp $ UnexpectedValue $ encodeString tag)

content :: forall a. Decoder a -> JPropDecoder a
content decoder = ReaderT \obj -> do
  contents <- obj .: contentsProp
  lmap (AtKey contentsProp) $ decode decoder contents

record
  :: forall rl ro ri
   . RowToList ri rl
  => RowListDecoder rl ri ro
  => String
  -> Record ri
  -> Decoder (Record ro)
record name decoders = ReaderT $
  lmap (Named name)
    <<< runReaderT (rowListDecoder (Proxy :: _ rl) decoders)
    <=< decodeJObject

tupleMap :: forall f a b. ToTupleDecoder f => (a -> b) -> f a -> TupleDecoder b
tupleMap f a = f <$> toTupleDecoder a

infixl 3 tupleMap as </$\>

tupleApply
  :: forall f a b
   . ToTupleDecoder f
  => TupleDecoder (a -> b)
  -> f a
  -> TupleDecoder b
tupleApply f a = f <*> toTupleDecoder a

infixl 3 tupleApply as </*\>

tupleConjoin
  :: forall f a b. ToTupleDecoder f => Decoder a -> f b -> TupleDecoder (a /\ b)
tupleConjoin d1 d2 = Tuple <$> toTupleDecoder d1 <*> toTupleDecoder d2

tuple :: forall a. TupleDecoder a -> Decoder a
tuple decoder = ReaderT $ map fst <<< flip (evalRWST decoder) 0 <=<
  decodeJArray

infixr 6 tupleConjoin as </\>

unit :: Decoder Unit
unit = ReaderT \json -> P.unit <$ decodeArray
  (Left <<< UnexpectedValue)
  json

null :: Decoder Unit
null = ReaderT decodeNull

decode :: forall a. Decoder a -> Json -> Either JsonDecodeError a
decode = runReaderT
