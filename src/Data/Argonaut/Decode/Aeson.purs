module Data.Argonaut.Decode.Aeson
  ( class RowListDecoder
  , Decoder
  , either
  , enum
  , hoistTuple
  , maybe
  , record
  , rowListDecoder
  , sumType
  , tuple
  , unit
  , value
  ) where

import Prelude hiding (unit)
import Prelude (unit) as P

import Control.Alt ((<|>))
import Control.Monad.RWS (RWSResult(..), RWST(..), evalRWST)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut.Aeson (contentsProp, leftProp, maybeToEither, rightProp, tagProp, unconsRecord)
import Data.Argonaut.Core (Json, jsonNull)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Argonaut.Decode.Decoders (decodeArray, decodeJArray, decodeJObject, decodeNull, decodeString)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Array (find, index)
import Data.Bifunctor (lmap)
import Data.Divide (divided)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object)
import Foreign.Object as Obj
import Prim.Row as R
import Prim.RowList (class RowToList, Cons, Nil)
import Record as Rec
import Type.Prelude (Proxy(..))

type Decoder = ReaderT Json (Either JsonDecodeError)
type JPropDecoder = ReaderT (Object Json) (Either JsonDecodeError)
type TupleDecoder = RWST (Array Json) Unit Int (Either JsonDecodeError)

infixr 4 divided as >*<

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

enum :: forall a. Show a => Array a -> Decoder a
enum values = ReaderT \json -> do
  v <- decodeString json
  maybeToEither (UnexpectedValue json) $ find ((v == _) <<< show) values

sumType :: forall a. Map String (Decoder a) -> Decoder a
sumType decoders = ReaderT \json -> do
  obj <- decodeJObject json
  tag <- obj .: tagProp
  contents <- obj .:? contentsProp .!= jsonNull
  decoder <- maybeToEither (wrongTag tag) $ Map.lookup tag decoders
  lmap (AtKey contentsProp) $ runReaderT decoder contents
  where
  wrongTag tag = AtKey tagProp (UnexpectedValue $ encodeString tag)

record
  :: forall rl ro ri
   . RowToList ri rl
  => RowListDecoder rl ri ro
  => String
  -> Record ri
  -> Decoder (Record ro)
record name decoders =
  ReaderT
    $
      lmap (Named name)
        <<< runReaderT (rowListDecoder (Proxy :: _ rl) decoders)
        <=< decodeJObject

hoistTuple :: forall a. Decoder a -> TupleDecoder a
hoistTuple decoder =
  RWST \arr i ->
    RWSResult
      (i + 1)
      <$>
        ( lmap (AtIndex i) $ runReaderT decoder =<< maybeToEither MissingValue
            (index arr i)
        )
      <*> pure P.unit

tuple :: forall a. TupleDecoder a -> Decoder a
tuple decoder = ReaderT $ map fst <<< flip (evalRWST decoder) 0 <=<
  decodeJArray

unit :: Decoder Unit
unit = ReaderT \json -> P.unit <$ decodeArray
  (Left <<< UnexpectedValue)
  json
