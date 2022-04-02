module Syntax.Tree 
  ( SExpr (..)
  ) where

import Data.Text (Text)

data SExpr
  = SInteger Integer
  | SBool Bool
  | SString String
  | SIdentifier Text
  | SSExpr [SExpr]
  deriving (Show)
