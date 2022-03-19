module Evaluator 
  ( eval
  , Value (..)
  , Context (..)
   ) where

import Data.Text (Text)
import Syntax.Tree

data Value
  = VInt Integer
  | VBool Bool
  | VNil
  deriving (Show)

type Context = [(Text, Value)]

findVar :: Text -> Context -> Value 
findVar x y =
  case lookup x y of
    Nothing -> error "variable not found"
    Just v -> v

eval :: Context -> SExpr -> (Context, Value)
eval ctx = \case
  SInteger n -> (ctx, VInt n)
  SBool b -> (ctx, VBool b)
  SIdentifier i -> (ctx, findVar i ctx)
  SSExpr xs -> (ctx, apply ctx xs)

evalList :: Context -> [SExpr] -> (Context, [Value])
evalList ctx = \case
  [] -> (ctx, [])
  (x : xs) ->
    let (ctx', v) = eval ctx x
        (ctx'', v') = evalList ctx' xs
     in (ctx'', v : v')

sum' :: Value -> Value -> Value
sum' (VInt x) (VInt y) = VInt (x + y)
sum' _ _ = error "is not an integer"

apply :: Context -> [SExpr] -> Value
apply ctx = \case
  (SIdentifier "+" : rest) ->
    let (_, vs) = evalList ctx rest
     in foldl sum' (VInt 0) vs
