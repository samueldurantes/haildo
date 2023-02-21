module Syntax.Tree
  ( SExpr (..)
  , Context
  , Value (..)
  ) where

import Data.Text (Text)
import Data.IORef (IORef)

data SExpr
  = SInteger Integer
  | SBool Bool
  | SString String
  | SIdentifier Text
  | SSExpr [SExpr]
  deriving (Show)

type Context = IORef [(Text, Value)]

data Value
  = VClosure Context Text SExpr
  | VInteger Integer
  | VBool Bool
  | VString String
  | VPrim (Context -> [SExpr] -> IO Value)
  | VNil

instance Show Value where
  show VClosure {}  = "(function)"
  show (VInteger i) = show i
  show (VBool b)    = show b
  show (VString s)  = s
  show (VPrim _)    = "(primitive)"
  show VNil         = "NIL"
