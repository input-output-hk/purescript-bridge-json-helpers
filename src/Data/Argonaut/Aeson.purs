module Data.Argonaut.Aeson
  ( class RowListDecoder
  , Decoder
  , Encoder
  , aeson
  , aesonEnum
  , aesonMaybe
  , aesonEither
  , aesonRecord
  , aesonSumType
  , aesonTuple
  , aesonUnit
  , rowListDecoder
  , t
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.RWS (RWSResult(..), RWST(..), evalRWST)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut.Core (Json, jsonNull)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Argonaut.Decode.Decoders (decodeArray, decodeJArray, decodeJObject, decodeNull, decodeString)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Array (find, index)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Op (Op)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (fst)
import Foreign.Object (Object, lookup)
import Prim.Row as R
import Prim.RowList (class RowToList, Cons, Nil)
import Record as Rec
import Record.Unsafe (unsafeSet)
import Type.Equality as TE
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type Decoder = ReaderT Json (Either JsonDecodeError)
type JPropDecoder = ReaderT (Object Json) (Either JsonDecodeError)
type TupleDecoder = RWST (Array Json) Unit Int (Either JsonDecodeError)
type Encoder = Op Json

class RowListDecoder :: forall k. k -> Row Type -> Row Type -> Constraint
class RowListDecoder rl ri ro | rl -> ri ro where
  rowListDecoder :: Proxy rl -> Record ri -> JPropDecoder (Record ro)

instance rowListDecoderNil :: RowListDecoder Nil () () where
  rowListDecoder _ _ = pure {}

recordPropDecoder
  :: forall r' r a (p :: Symbol)
   . R.Cons p a r r'
  => IsSymbol p
  => Proxy p
  -> Decoder a
  -> JPropDecoder (Record r)
  -> JPropDecoder (Record r')
recordPropDecoder p decodeA decodeR = ReaderT \obj -> do
  let key = reflectSymbol p
  r <- runReaderT decodeR obj
  a <- lmap (AtKey key) case lookup key obj of
    Just val -> runReaderT decodeA val
    Nothing -> Left MissingValue
  pure $ unsafeSet key a r

instance rowListDecoderDecoderCons ::
  ( RowListDecoder rs ri' ro'
  , R.Cons sym (Decoder a) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  , TE.TypeEquals co (Decoder a)
  ) =>
  RowListDecoder (Cons sym co rs) ri ro where
  rowListDecoder _ decoders = recordPropDecoder (Proxy :: _ sym) head tail
    where
    head :: Decoder a
    head = TE.from (Rec.get (Proxy :: _ sym) decoders)

    tail :: JPropDecoder (Record ro')
    tail = rowListDecoder (Proxy :: _ rs)
      ((unsafeCoerce :: Record ri -> Record ri') decoders)

aeson :: forall a. DecodeJson a => Decoder a
aeson = ReaderT $ decodeJson

aesonMaybe :: forall a. Decoder a -> Decoder (Maybe a)
aesonMaybe decoder =
  Nothing <$ ReaderT decodeNull <|> Just <$> decoder

aesonEither :: forall a b. Decoder a -> Decoder b -> Decoder (Either a b)
aesonEither decoderA decoderB = ReaderT $ decodeJObject >=> decodeEitherObj
  where
  decodeEitherObj obj =
    Left <$> decodeLeft obj <|> Right <$> decodeRight obj
  decodeLeft obj = obj .: leftProp >>= runReaderT decoderA
  decodeRight obj = obj .: rightProp >>= runReaderT decoderB

aesonEnum :: forall a. Show a => Array a -> Decoder a
aesonEnum values = ReaderT \json -> do
  value <- decodeString json
  maybeToEither (UnexpectedValue json) $ find ((value == _) <<< show) values

aesonSumType :: forall a. Map String (Decoder a) -> Decoder a
aesonSumType decoders = ReaderT \json -> do
  obj <- decodeJObject json
  tag <- obj .: tagProp
  contents <- obj .:? contentsProp .!= jsonNull
  decoder <- maybeToEither (wrongTag tag) $ Map.lookup tag decoders
  lmap (AtKey contentsProp) $ runReaderT decoder contents
  where
  wrongTag tag = AtKey tagProp (UnexpectedValue $ encodeString tag)

aesonRecord
  :: forall rl ro ri
   . RowToList ri rl
  => RowListDecoder rl ri ro
  => String
  -> Record ri
  -> Decoder (Record ro)
aesonRecord name decoders =
  ReaderT
    $
      lmap (Named name)
        <<< runReaderT (rowListDecoder (Proxy :: _ rl) decoders)
        <=< decodeJObject

t :: forall a. Decoder a -> TupleDecoder a
t decoder =
  RWST \arr i ->
    RWSResult
      (i + 1)
      <$>
        ( lmap (AtIndex i) $ runReaderT decoder =<< maybeToEither MissingValue
            (index arr i)
        )
      <*> pure unit

aesonTuple :: forall a. TupleDecoder a -> Decoder a
aesonTuple decoder = ReaderT $ map fst <<< flip (evalRWST decoder) 0 <=<
  decodeJArray

aesonUnit :: Decoder Unit
aesonUnit = ReaderT \json -> unit <$ decodeArray (Left <<< UnexpectedValue) json

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
