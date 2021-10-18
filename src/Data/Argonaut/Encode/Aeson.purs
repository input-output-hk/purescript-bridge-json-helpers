module Data.Argonaut.Encode.Aeson
  ( class RowListEncoder
  , Encoder
  , value
  , hoistTuple
  , either
  , enum
  , maybe
  , null
  , record
  , sumType
  , tuple
  , unit
  , rowListEncoder
  , (>*<)
  ) where

import Prelude

import Data.Argonaut.Aeson (tagProp, unconsRecord)
import Data.Argonaut.Core (Json, fromArray, fromObject, jsonEmptyArray, jsonEmptyObject, jsonNull)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Divide (divided)
import Data.Either (Either(..))
import Data.Functor.Contravariant (cmap)
import Data.Maybe (Maybe(..))
import Data.Semigroup.Last (Last(..))
import Data.Newtype (un)
import Data.Op (Op(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as Obj
import Prim.Row as R
import Prim.RowList (class RowToList, Cons, Nil)
import Type.Prelude (Proxy(..))

type Encoder = Op Json
type JPropEncoder = Op (Object (Last Json))
type TupleEncoder = Op (Array Json)

infixr 4 divided as >*<

infixr 4 cmap as >$<

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
  Op $ Obj.singleton (reflectSymbol p) <<< Last <<< un Op encoder

value :: forall a. EncodeJson a => Encoder a
value = Op $ encodeJson

maybe :: forall a. Encoder a -> Encoder (Maybe a)
maybe encoder = Op case _ of
  Just a -> un Op encoder a
  Nothing -> jsonNull

either :: forall a b. Encoder a -> Encoder b -> Encoder (Either a b)
either encoderA encoderB = Op case _ of
  Left a -> un Op encoderA a
  Right b -> un Op encoderB b

enum :: forall a. Show a => Encoder a
enum = Op $ encodeString <<< show

sumType
  :: forall a
   . (a -> forall r. (forall p. String /\ p /\ (Encoder p) -> r) -> r)
  -> Encoder a
sumType getEncoder = Op \a ->
  getEncoder a \(tag /\ contents /\ encoder) ->
    tagProp := tag ~> "contents" := un Op encoder contents ~> jsonEmptyObject

record
  :: forall rl ro ri
   . RowToList ri rl
  => RowListEncoder rl ri ro
  => Record ri
  -> Encoder (Record ro)
record encoders =
  Op $ fromObject <<< map (un Last) <<< un Op
    (rowListEncoder (Proxy :: _ rl) encoders)

hoistTuple :: forall a. Encoder a -> TupleEncoder a
hoistTuple decoder = Op $ pure <<< un Op decoder

tuple :: forall a. TupleEncoder a -> Encoder a
tuple encoder = Op \a -> fromArray $ un Op encoder a

unit :: Encoder Unit
unit = Op $ const jsonEmptyArray

null :: Encoder Unit
null = Op $ const jsonNull
