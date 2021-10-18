module Data.Argonaut.Aeson where


import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Lacks)
import Record as Rec
import Type.Prelude (Proxy)

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

unconsRecord
  :: forall (prop :: Symbol) (a :: Type) (rl :: Row Type) (rt :: Row Type)
   . IsSymbol prop
  => Lacks prop rt
  => Cons prop a rt rl
  => Proxy prop
  -> Record rl
  -> Tuple a (Record rt)
unconsRecord p record = Tuple (Rec.get p record) (Rec.delete p record)
