module Syntax.Tree
  ( SExpr (..)
  , Context(..)
  , Value (..)
  , Ret(..)
  ) where

import Data.Char (toLower)
import Data.Text (Text)
import Data.IORef (IORef)
import System.Posix (DL)
import Foreign (FunPtr)

data Ret = TInteger | TString | TBool

data SExpr
  = SInteger Integer
  | SBool Bool
  | SString String
  | SIdentifier Text
  | SSExpr [SExpr]
  deriving (Show)

data Context = Context
  { globals :: IORef [(Text, Value)]
  , locals :: [(Text, Value)]
  }

data Value
  = VClosure Context Text SExpr
  | VInteger Integer
  | VBool Bool
  | VString String
  | VPrim (Context -> [SExpr] -> IO Value)
  | VPtr DL
  | VFunPtr (FunPtr ()) Ret
  | VNil

instance Show Value where
  show VClosure {}   = "(function)"
  show (VInteger i)  = show i
  show (VBool b)     = map toLower $ show b
  show (VString s)   = s
  show (VPrim _)     = "(primitive)"
  show (VPtr _)      = "(ptr)"
  show (VFunPtr _ _) = "(fun_ptr)"
  show VNil          = "NIL"
