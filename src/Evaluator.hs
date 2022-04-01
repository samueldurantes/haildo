module Evaluator
  ( eval
  , Context (..)
  ) where

import Data.Text (Text)
import Syntax.Tree
import Data.Map (Map)
import Control.Monad.State
import Data.Foldable (traverse_)

import qualified Data.Map as Map
import qualified Control.Monad.State as State
import Debug.Trace (traceM, traceShowM)

data Function = Function
  { args :: [Text]
  , body :: [SExpr]
  } deriving (Show)

data Value
  = VInteger Integer
  | VBool Bool
  | VFunction Function
  | VNil

data Context = Context
  { globalContext :: Map Text Value
  , stackContext :: [Map Text Value]
  }

instance Show Value where
  show = \case
    VInteger n -> show n
    VBool b -> show b
    VFunction func -> "[Function]"
    VNil -> "nil"

findVariable :: (MonadState Context m) => Text -> m Value
findVariable varName = do
  ctx <- State.get
  if null $ stackContext ctx
    then searchGlobal ctx
    else maybe (searchGlobal ctx) pure (Map.lookup varName (head $ stackContext ctx))
  where
    cannotFind = error $ "Variable '" ++ show varName ++ "' not found"
    searchGlobal ctx = maybe cannotFind pure (Map.lookup varName (globalContext ctx))

addVariable :: (MonadState Context m) => Text -> Value -> m ()
addVariable key value = State.modify $ \ctx ->
  if null (stackContext ctx)
     then ctx { globalContext = Map.insert key value (globalContext ctx) }
     else ctx { stackContext = Map.insert key value (head (stackContext ctx)) : tail (stackContext ctx)}

addFunction :: (MonadState Context m) => Text -> [Text] -> [SExpr] -> m ()
addFunction name args body = State.modify $ \ctx ->
  ctx { globalContext = Map.insert name value (globalContext ctx)}
    where
      value = VFunction $ Function args body

pushStack :: (MonadState Context m) => m ()
pushStack = State.modify $ \ctx ->
  ctx { stackContext = Map.empty : stackContext ctx }

popStack :: (MonadState Context m) => m ()
popStack = State.modify $ \ctx ->
  ctx { stackContext = tail (stackContext ctx) }

f :: [SExpr] -> [Text]
f [] = []
f (SIdentifier x : xs) = x : f xs
f _ = error "Expected a SIdentifier"

eval :: (MonadState Context m, MonadIO m) => SExpr -> m Value
eval = \case
  SInteger i -> pure $ VInteger i
  SBool b -> pure $ VBool b
  SIdentifier i -> findVariable i
  SSExpr xs -> apply xs

isInt :: Value -> Integer
isInt = \case
  VInteger r -> r
  _ -> error "Expected a integer"

apply :: (MonadState Context m, MonadIO m) => [SExpr] -> m Value
apply = \case
  (SIdentifier "+" : rest) -> do
    values <- map isInt <$> traverse eval rest
    pure (VInteger $ sum values)
  (SIdentifier "-" : rest) -> do
    values <- map isInt <$> traverse eval rest
    pure (VInteger $ foldl1 (-) values)
  (SIdentifier "print" : rest) -> do
    res <- traverse eval rest
    liftIO $ putStrLn $ unwords $ map show res
    pure VNil
  [SIdentifier "let", SIdentifier x, y] ->
    eval y >>= addVariable x >> pure VNil
  [SIdentifier "<", a, b] -> do
    res <- isInt <$> eval a
    res2 <- isInt <$> eval b
    pure (VBool (res < res2))
  [SIdentifier "if", cond, if', else'] -> do
    res <- eval cond
    case res of
      VBool True  -> eval if'
      VBool False -> eval else'
      _ -> error "Lol"
  (SIdentifier "function" : SIdentifier name : SSExpr params : body) ->
    addFunction name (f params) body >> pure VNil
  (fn : args) -> do
    function <- eval fn
    argsV <- traverse eval args
    case function of
      VFunction (Function params body) -> do
        pushStack
        traverse_ (uncurry addVariable) (zip params argsV)
        traceShowM (zip params argsV)
        res <- traverse eval body
        popStack
        pure $ last res
      _ -> error "Not a function"
  _ -> error "Not implemented yet"
