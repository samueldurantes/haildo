module Evaluator 
  ( eval
  , Context (..)
  ) where

import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Text (Text)
import Syntax.Tree (SExpr(..))

import qualified Control.Monad.State as State
import qualified Data.Map as Map

data Function = Function
  { args :: [Text] 
  , body :: [SExpr]
  } deriving (Show)

data Context = Context
  { globalContext :: Map Text Value
  , stackContext :: [Map Text Value]
  }

data Value
  = VInteger Integer
  | VBool Bool
  | VFunction Function
  | VNil
  deriving (Eq)

instance Eq Function where 
  (==) _ _ = False

instance Show Value where
  show = \case
    VInteger n  -> show n
    VBool b     -> show b
    VFunction _ -> "[Function]"
    VNil        -> "nil"

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
    else ctx { stackContext = Map.insert key value (head (stackContext ctx)) : tail (stackContext ctx) }

addFunction :: (MonadState Context m) => Text -> [Text] -> [SExpr] -> m ()
addFunction name args body = State.modify $ \ctx ->
  ctx { globalContext = Map.insert name value (globalContext ctx) }
  where
    value = VFunction $ Function args body

pushStack :: (MonadState Context m) => m ()
pushStack = State.modify $ \ctx ->
  ctx { stackContext = Map.empty : stackContext ctx }

popStack :: (MonadState Context m) => m ()
popStack = State.modify $ \ctx ->
  ctx { stackContext = tail (stackContext ctx) }

filterIdentifier :: [SExpr] -> [Text]
filterIdentifier [] = []
filterIdentifier (SIdentifier x : xs) = x : filterIdentifier xs
filterIdentifier _ = error "Expected a SIdentifier"

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

applyFun :: (MonadState Context m, MonadIO m) => Value -> [SExpr] -> m Value
applyFun fun args = do
  argsV <- traverse eval args
  case fun of
    VFunction (Function params body) -> do
      pushStack
      traverse_ (uncurry addVariable) (zip params argsV)
      res <- traverse eval body
      popStack
      pure $ last res
    _ -> error "Not a function"

applyOp :: (MonadState Context m, MonadIO m) => SExpr -> SExpr -> (Integer -> Integer  -> Value) -> m Value
applyOp a b fn = do 
  res <- isInt <$> eval a
  res2 <- isInt <$> eval b
  pure (fn res res2)

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)

builtin :: (MonadState Context m, MonadIO m) => Text -> [SExpr] -> m Value

builtin "+" exprs = do
  values <- traverse (fmap isInt . eval) exprs
  pure $ VInteger $ sum values

builtin "-" exprs = do
  values <- traverse (fmap isInt . eval) exprs
  pure $ VInteger $ foldl1 (-) values

builtin "*" exprs = do
  values <- traverse (fmap isInt . eval) exprs
  pure $ VInteger $ product values

builtin "<" [a, b] = applyOp a b (VBool .: (<))
builtin ">" [a, b] = applyOp a b (VBool .: (>))

builtin "print" exprs = do
  res <- traverse eval exprs
  liftIO $ putStrLn $ unwords $ map show res
  pure VNil

builtin "let" [SIdentifier x, y] = do
  addVariable x =<< eval y
  pure VNil

builtin "if" [cond, if', else'] = do
  res <- eval cond
  case res of
    VBool True -> eval if'
    VBool False -> eval else'
    _ -> error "Lol"

builtin "function" (SIdentifier name : SSExpr params : body) = do
  addFunction name (filterIdentifier params) body
  pure VNil

builtin "assert" [SIdentifier name, a, b] = do
  resA <- eval a 
  resB <- eval b
  if resA == resB
    then liftIO $ putStrLn $ concat ["\n\x1b[1m\x1b[32m  ⊙ ", show name, " succeded!\x1b[0m"]
    else liftIO $ putStrLn $ concat ["\n\x1b[1m\x1b[31m  ⊙ ", show name, " failed!\n\x1b[0m", replicate 7 ' ', "Expected: ", show resB,"\n", replicate 7 ' ', "Got: ", show resA]
  pure VNil

builtin other args = do
  function <- findVariable other
  applyFun function args

apply :: (MonadState Context m, MonadIO m) => [SExpr] -> m Value
apply = \case
  (SIdentifier name : rest) -> builtin name rest
  (fn : args) -> do
    function <- eval fn
    applyFun function args
  [] -> error "Impossible!"
