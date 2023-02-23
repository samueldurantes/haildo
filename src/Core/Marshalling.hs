module Core.Marshalling where

import Foreign.LibFFI
import Syntax.Tree (Value(..))

marshal :: Value -> Arg
marshal = \case
  VInteger x -> argCInt (fromIntegral x)
  VBool x    -> if x then argCInt 1 else argCInt 0
  VString x  -> argString x
  _          -> error "Cannot marshal"
