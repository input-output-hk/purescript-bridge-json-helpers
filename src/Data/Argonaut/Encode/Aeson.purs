module Data.Argonaut.Encode.Aeson
  ( class RowListEncoder
  , class ToTupleEncoder
  , Encoder
  , dictionary
  , either
  , encode
  , enum
  , maybe
  , null
  , record
  , rowListEncoder
  , encodeTagged
  , tuple
  , toTupleEncoder
  , tupleDivided
  , unit
  , value
  , (>*<)
  , (>$<)
  , (>/\<)
  ) where

import Prelude

import Data.Argonaut.Aeson (contentsProp, leftProp, rightProp, tagProp, unconsRecord)
import Data.Argonaut.Core (Json, caseJson, fromArray, fromObject, jsonEmptyArray, jsonEmptyObject, jsonNull)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (ltraverse)
import Data.Divide (divided)
import Data.Either (Either(..))
import Data.Functor.Contravariant (cmap)
import Data.Map (Map, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Op (Op(..))
import Data.Semigroup.Last (Last(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object, fromFoldable)
import Foreign.Object as Obj
import Prim.Row as R
import Prim.RowList (class RowToList, Cons, Nil)
import Type.Prelude (Proxy(..))

type Encoder = Op Json
type JPropEncoder = Op (Object (Last Json))
type TupleEncoder = Op (Array Json)

infixr 4 divided as >*<

infixr 4 cmap as >$<

class ToTupleEncoder f where
  toTupleEncoder :: forall a. f a -> TupleEncoder a

instance toTupleEncoderEncoder :: ToTupleEncoder (Op Json) where
  toTupleEncoder = mapEncoder Array.singleton

instance toTupleEncoderTupleEncoder :: ToTupleEncoder (Op (Array Json)) where
  toTupleEncoder = identity

class RowListEncoder :: forall k. k -> Row Type -> Row Type -> Constraint
class RowListEncoder rl ri ro | rl -> ri ro where
  rowListEncoder :: Proxy rl -> Record ri -> JPropEncoder (Record ro)

instance rowListEncoderNil :: RowListEncoder Nil () () where
  rowListEncoder _ _ = Op $ const Obj.empty

instance rowListEncoderEncoderCons ::
  ( IsSymbol prop
  , R.Lacks prop tailEncoders
  , R.Cons prop (Encoder a) tailEncoders encoders
  , R.Lacks prop tailValues
  , R.Cons prop a tailValues values
  , RowListEncoder tail tailEncoders tailValues
  ) =>
  RowListEncoder (Cons prop (Encoder a) tail) encoders values where
  rowListEncoder _ encoders =
    let
      Tuple valueEncoder tailEncoders = unconsRecord (Proxy :: _ prop) encoders
    in
      unconsRecord (Proxy :: _ prop)
        >$< propEncoder (Proxy :: _ prop) valueEncoder
        >*< rowListEncoder (Proxy :: _ tail) tailEncoders

propEncoder
  :: forall p a
   . IsSymbol p
  => Proxy p
  -> Encoder a
  -> JPropEncoder a
propEncoder p encoder =
  Op $ Obj.singleton (reflectSymbol p) <<< Last <<< encode encoder

value :: forall a. EncodeJson a => Encoder a
value = Op $ encodeJson

maybe :: forall a. Encoder a -> Encoder (Maybe a)
maybe encoder = Op case _ of
  Just a -> encode encoder a
  Nothing -> jsonNull

either :: forall a b. Encoder a -> Encoder b -> Encoder (Either a b)
either encoderA encoderB = Op case _ of
  Left a -> leftProp := encode encoderA a ~> jsonEmptyObject
  Right b -> rightProp := encode encoderB b ~> jsonEmptyObject

dictionary :: forall a b. Encoder a -> Encoder b -> Encoder (Map a b)
dictionary encoderA encoderB = Op $ toPairs >>> encodePairs
  where
  toPairs :: Map a b -> Array (Tuple Json Json)
  toPairs = map (bimap (encode encoderA) (encode encoderB)) <<< toUnfoldable
  encodePairs :: Array (Tuple Json Json) -> Json
  encodePairs pairs = case traverse (ltraverse tryString) pairs of
    Nothing -> encodeJson pairs
    Just pairs' -> fromObject $ fromFoldable pairs'
  tryString =
    caseJson
      (const Nothing) 
      (const Nothing) 
      (const Nothing) 
      (Just) 
      (const Nothing) 
      (const Nothing)


enum :: forall a. Show a => Encoder a
enum = Op $ encodeString <<< show

record
  :: forall rl ro ri
   . RowToList ri rl
  => RowListEncoder rl ri ro
  => Record ri
  -> Encoder (Record ro)
record encoders =
  mapEncoder (fromObject <<< map unwrap) $ rowListEncoder (Proxy :: _ rl)
    encoders

tupleDivided
  :: forall f a b. ToTupleEncoder f => Encoder a -> f b -> TupleEncoder (a /\ b)
tupleDivided encoder = divided (toTupleEncoder encoder) <<< toTupleEncoder

infixr 6 tupleDivided as >/\<

tuple :: forall a. TupleEncoder a -> Encoder a
tuple = over Op $ compose fromArray

unit :: Encoder Unit
unit = Op $ const jsonEmptyArray

null :: Encoder Unit
null = Op $ const jsonNull

encode :: forall a b. Op a b -> b -> a
encode = unwrap

encodeTagged :: forall a. String -> a -> Encoder a -> Json
encodeTagged tag a encoder =
  tagProp := tag ~> contentsProp := encode encoder a ~> jsonEmptyObject

mapEncoder :: forall a b c. (a -> b) -> Op a c -> Op b c
mapEncoder = over Op <<< map
