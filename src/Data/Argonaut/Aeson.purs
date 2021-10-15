module Data.Argonaut.Aeson
  ( class RowListDecoder
  , class RowListEncoder
  , Decoder
  , Encoder
  , decodeAeson
  , decodeAesonEither
  , decodeAesonEnum
  , decodeAesonMaybe
  , decodeAesonRecord
  , decodeAesonSumType
  , decodeAesonTuple
  , decodeAesonUnit
  , encodeAeson
  , encodeAesonEither
  , encodeAesonEnum
  , encodeAesonMaybe
  , encodeAesonNull
  , encodeAesonRecord
  , encodeAesonSumType
  , encodeAesonTuple
  , encodeAesonUnit
  , rowListDecoder
  , rowListEncoder
  , td
  , te
  , (>*<)
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.RWS (RWSResult(..), RWST(..), evalRWST)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut.Core (Json, fromArray, fromObject, jsonEmptyArray, jsonEmptyObject, jsonNull)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Argonaut.Decode.Decoders (decodeArray, decodeJArray, decodeJObject, decodeNull, decodeString)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Array (find, index)
import Data.Bifunctor (lmap)
import Data.Divide (divide, divided)
import Data.Either (Either(..))
import Data.List (List)
import Data.List as L
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Op (Op(..))
import Data.Profunctor.Strong ((&&&))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object, lookup)
import Foreign.Object as FO
import Prim.Row as R
import Prim.RowList (class RowToList, Cons, Nil)
import Record as Rec
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Equality as TE
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type Decoder = ReaderT Json (Either JsonDecodeError)
type JPropDecoder = ReaderT (Object Json) (Either JsonDecodeError)
type TupleDecoder = RWST (Array Json) Unit Int (Either JsonDecodeError)
type Encoder = Op Json
type JPropEncoder = Op (List (Tuple String Json))
type TupleEncoder = Op (Array Json)

infixr 4 divided as >*<

class RowListDecoder :: forall k. k -> Row Type -> Row Type -> Constraint
class RowListDecoder rl ri ro | rl -> ri ro where
  rowListDecoder :: Proxy rl -> Record ri -> JPropDecoder (Record ro)

instance rowListDecoderNil :: RowListDecoder Nil () () where
  rowListDecoder _ _ = pure {}

instance rowListDecoderDecoderCons ::
  ( RowListDecoder rs ri' ro'
  , R.Cons sym (Decoder a) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  , TE.TypeEquals co (Decoder a)
  ) =>
  RowListDecoder (Cons sym co rs) ri ro where
  rowListDecoder _ decoders = ReaderT \obj -> do
    let key = reflectSymbol (Proxy :: _ sym)
    r <- runReaderT decodeR obj
    a <- lmap (AtKey key) case lookup key obj of
      Just val -> runReaderT decodeA val
      Nothing -> Left MissingValue
    pure $ unsafeSet key a r
    where
    decodeA :: Decoder a
    decodeA = TE.from (Rec.get (Proxy :: _ sym) decoders)

    decodeR :: JPropDecoder (Record ro')
    decodeR = rowListDecoder (Proxy :: _ rs) (unsafeCoerce decoders)

class RowListEncoder :: forall k. k -> Row Type -> Row Type -> Constraint
class RowListEncoder rl ri ro | rl -> ri ro where
  rowListEncoder :: Proxy rl -> Record ri -> JPropEncoder (Record ro)

instance rowListEncoderNil :: RowListEncoder Nil () () where
  rowListEncoder _ _ = Op $ const L.Nil

instance rowListEncoderEncoderCons ::
  ( RowListEncoder rs ri' ro'
  , R.Cons sym (Encoder a) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  , TE.TypeEquals co (Encoder a)
  ) =>
  RowListEncoder (Cons sym co rs) ri ro where
  rowListEncoder _ encoders =
    divide (unsafeGet key &&& unsafeCoerce) encodeA encodeR
    where
    key = reflectSymbol (Proxy :: _ sym)

    encodeA :: JPropEncoder a
    encodeA = Op $ pure <<< Tuple key <<<
      (un Op $ TE.from (Rec.get (Proxy :: _ sym) encoders))

    encodeR :: JPropEncoder (Record ro')
    encodeR = rowListEncoder (Proxy :: _ rs)
      ((unsafeCoerce :: Record ri -> Record ri') encoders)

decodeAeson :: forall a. DecodeJson a => Decoder a
decodeAeson = ReaderT $ decodeJson

encodeAeson :: forall a. EncodeJson a => Encoder a
encodeAeson = Op $ encodeJson

decodeAesonMaybe :: forall a. Decoder a -> Decoder (Maybe a)
decodeAesonMaybe decoder =
  Nothing <$ ReaderT decodeNull <|> Just <$> decoder

encodeAesonMaybe :: forall a. Encoder a -> Encoder (Maybe a)
encodeAesonMaybe encoder = Op case _ of
  Just a -> un Op encoder a
  Nothing -> jsonNull

decodeAesonEither :: forall a b. Decoder a -> Decoder b -> Decoder (Either a b)
decodeAesonEither decoderA decoderB = ReaderT $ decodeJObject >=>
  decodeEitherObj
  where
  decodeEitherObj obj =
    Left <$> decodeLeft obj <|> Right <$> decodeRight obj
  decodeLeft obj = obj .: leftProp >>= runReaderT decoderA
  decodeRight obj = obj .: rightProp >>= runReaderT decoderB

encodeAesonEither :: forall a b. Encoder a -> Encoder b -> Encoder (Either a b)
encodeAesonEither encoderA encoderB = Op case _ of
  Left a -> un Op encoderA a
  Right b -> un Op encoderB b

decodeAesonEnum :: forall a. Show a => Array a -> Decoder a
decodeAesonEnum values = ReaderT \json -> do
  value <- decodeString json
  maybeToEither (UnexpectedValue json) $ find ((value == _) <<< show) values

encodeAesonEnum :: forall a. Show a => Encoder a
encodeAesonEnum = Op $ encodeString <<< show

decodeAesonSumType :: forall a. Map String (Decoder a) -> Decoder a
decodeAesonSumType decoders = ReaderT \json -> do
  obj <- decodeJObject json
  tag <- obj .: tagProp
  contents <- obj .:? contentsProp .!= jsonNull
  decoder <- maybeToEither (wrongTag tag) $ Map.lookup tag decoders
  lmap (AtKey contentsProp) $ runReaderT decoder contents
  where
  wrongTag tag = AtKey tagProp (UnexpectedValue $ encodeString tag)

encodeAesonSumType
  :: forall a
   . (a -> forall r. (forall p. String /\ p /\ (Encoder p) -> r) -> r)
  -> Encoder a
encodeAesonSumType getEncoder = Op \a ->
  getEncoder a \(tag /\ contents /\ encoder) ->
    "tag" := tag ~> "contents" := un Op encoder contents ~> jsonEmptyObject

decodeAesonRecord
  :: forall rl ro ri
   . RowToList ri rl
  => RowListDecoder rl ri ro
  => String
  -> Record ri
  -> Decoder (Record ro)
decodeAesonRecord name decoders =
  ReaderT
    $
      lmap (Named name)
        <<< runReaderT (rowListDecoder (Proxy :: _ rl) decoders)
        <=< decodeJObject

encodeAesonRecord
  :: forall rl ro ri
   . RowToList ri rl
  => RowListEncoder rl ri ro
  => Record ri
  -> Encoder (Record ro)
encodeAesonRecord encoders =
  Op $ fromObject <<< FO.fromFoldable <<< un Op
    (rowListEncoder (Proxy :: _ rl) encoders)

td :: forall a. Decoder a -> TupleDecoder a
td decoder =
  RWST \arr i ->
    RWSResult
      (i + 1)
      <$>
        ( lmap (AtIndex i) $ runReaderT decoder =<< maybeToEither MissingValue
            (index arr i)
        )
      <*> pure unit

te :: forall a. Encoder a -> TupleEncoder a
te decoder = Op $ pure <<< un Op decoder

decodeAesonTuple :: forall a. TupleDecoder a -> Decoder a
decodeAesonTuple decoder = ReaderT $ map fst <<< flip (evalRWST decoder) 0 <=<
  decodeJArray

encodeAesonTuple :: forall a. TupleEncoder a -> Encoder a
encodeAesonTuple encoder = Op \a -> fromArray $ un Op encoder a

decodeAesonUnit :: Decoder Unit
decodeAesonUnit = ReaderT \json -> unit <$ decodeArray
  (Left <<< UnexpectedValue)
  json

encodeAesonUnit :: Encoder Unit
encodeAesonUnit = Op $ const jsonEmptyArray

encodeAesonNull :: Encoder Unit
encodeAesonNull = Op $ const jsonNull

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

leftProp :: String
leftProp = "Left"

rightProp :: String
rightProp = "Right"

tagProp :: String
tagProp = "tag"

contentsProp :: String
contentsProp = "contents"

maybeToEither :: forall a b. a -> Maybe b -> Either a b
maybeToEither a = maybe (Left a) Right
