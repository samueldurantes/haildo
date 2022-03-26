module Evaluator 
  ( eval
  , evalList
  , Value (..)
  , Context (..)
   ) where

import Data.Text (Text)
import Syntax.Tree

data Function = Function 
  { args :: [Text]
  , body :: [SExpr] 
  } deriving (Show)

data Value
  = VInt Integer
  | VBool Bool
  | VFunction Function
  | VNil
  deriving (Show)

type Context = [(Text, Value)]

findVar :: Text -> Context -> Value 
findVar x y =
  case lookup x y of
    Nothing -> error "variable not found"
    Just v -> v

f :: [SExpr] -> [Text]
f [] = []
f (SIdentifier x : xs) = x : f xs 
f _ = error "Expected Identifier"

eval :: Context -> SExpr -> (Context, Value)
eval ctx = \case
  SInteger n -> (ctx, VInt n)
  SBool b -> (ctx, VBool b)
  SIdentifier i -> (ctx, findVar i ctx)
  SSExpr [SIdentifier "let", SIdentifier x, y] ->
    let (ctx', v) = eval ctx y
     in ((x, v) : ctx', VNil)
  SSExpr (SIdentifier "function" : SIdentifier name : SSExpr args : body) -> 
    ((name, VFunction (Function (f args) body)) : ctx, VNil)
  SSExpr xs -> (ctx, apply ctx xs)

evalList :: Context -> [SExpr] -> (Context, [Value])
evalList ctx = \case
  [] -> (ctx, [])
  (x : xs) ->
    let (ctx', v) = eval ctx x
        (ctx'', v') = evalList ctx' xs
     in (ctx'', v : v')

evalFunction :: Context -> [SExpr] -> (Context, [Value])
evalFunction ctx = \case
  [] -> (ctx, [])
  (x : args) ->
    let (ctx', v) = eval ctx x
     in case v of
          VFunction (Function params body) ->
            if length params == length args
               then let (ctx'', v') = evalList ctx' args
                        function_ctx = zip params v' ++ ctx''
                     in evalList function_ctx body
               else error $ "Expected: " ++ show (length params) ++ " arguments, but instead got " ++ show (length args)
          _ -> error "not is a function"

sum' :: Value -> Value -> Value
sum' (VInt x) (VInt y) = VInt (x + y)
sum' _ _ = error "is not an integer"

apply :: Context -> [SExpr] -> Value
apply ctx = \case
  (SIdentifier "+" : rest) ->
    let (_, vs) = evalList ctx rest
     in foldl sum' (VInt 0) vs
  other -> last $ snd $ evalFunction ctx other
