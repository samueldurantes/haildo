module Core.Primitives
  ( primitives
  ) where

import Prelude hiding (print)

import Core.Interpreter (eval)
import Data.Text (Text)
import Data.IORef (modifyIORef)
import Syntax.Tree (SExpr(..), Value(..), Context(..), Ret(..))
import System.Posix (dlopen, RTLDFlags (RTLD_LAZY), dlsym)

isInt :: Value -> Bool
isInt (VInteger _) = True
isInt _ = False

toInt :: Value -> Integer
toInt (VInteger i) = i
toInt _ = error "not is a number"

add :: Context -> [SExpr] -> IO Value
add ctx (SIdentifier "+" : rest) = do
  xs <- traverse (eval ctx) rest
  if all isInt xs
    then pure $ VInteger $ sum $ map toInt xs
    else error "illegal function call"
add _ _ = error "illegal function call"

sub :: Context -> [SExpr] -> IO Value
sub ctx (SIdentifier "-" : rest) = do
  xs <- traverse (eval ctx) rest
  if all isInt xs
    then pure $ VInteger $ foldl1 (-) $ map toInt xs
    else error "illegal function call"
sub _ _ = error "illegal function call"

mul :: Context -> [SExpr] -> IO Value
mul ctx (SIdentifier "*" : rest) = do
  xs <- traverse (eval ctx) rest
  if all isInt xs
    then pure $ VInteger $ product $ map toInt xs
    else error "illegal function call"
mul _ _ = error "illegal function call"

eql :: Context -> [SExpr] -> IO Value
eql ctx [SIdentifier "=", e1, e2] = do
  (VInteger i1) <- eval ctx e1
  (VInteger i2) <- eval ctx e2
  pure $ VBool $ i1 == i2
eql _ _ = error "illegal function call"

-- (<)
ltn :: Context -> [SExpr] -> IO Value
ltn ctx [SIdentifier "<", e1, e2] = do
  (VInteger i1) <- eval ctx e1
  (VInteger i2) <- eval ctx e2
  pure $ VBool $ i1 < i2
ltn _ _ = error "illegal function call"

-- (<=)
lte :: Context -> [SExpr] -> IO Value
lte ctx [SIdentifier "<=", e1, e2] = do
  (VInteger i1) <- eval ctx e1
  (VInteger i2) <- eval ctx e2
  pure $ VBool $ i1 <= i2
lte _ _ = error "illegal function call"

-- (>)
gtn :: Context -> [SExpr] -> IO Value
gtn ctx [SIdentifier ">", e1, e2] = do
  (VInteger i1) <- eval ctx e1
  (VInteger i2) <- eval ctx e2
  pure $ VBool $ i1 > i2
gtn _ _ = error "illegal function call"

-- (>=)
gte :: Context -> [SExpr] -> IO Value
gte ctx [SIdentifier ">", e1, e2] = do
  (VInteger i1) <- eval ctx e1
  (VInteger i2) <- eval ctx e2
  pure $ VBool $ i1 >= i2
gte _ _ = error "illegal function call"

def :: Context -> [SExpr] -> IO Value
def ctx [SIdentifier "define", SIdentifier n, body] = do
  b <- eval ctx body
  modifyIORef ctx.globals (\ctx' -> (n, b) : ctx')
  pure VNil
def _ _ = error "illegal function call"

print :: Context -> [SExpr] -> IO Value
print ctx (SIdentifier "print" : rest) = do
  b <- traverse (eval ctx) rest
  putStrLn $ unwords $ map show b
  pure VNil
print _ _ = error "illegal function call"

lambda :: Context -> [SExpr] -> IO Value
lambda ctx [SIdentifier "lambda", SSExpr [SIdentifier p], body] = do
  pure $ VClosure ctx p body
lambda _ _ = error "illegal function call"

if_ :: Context -> [SExpr] -> IO Value
if_ ctx [SIdentifier "if", t, e1, e2] = do
  (VBool b) <- eval ctx t
  if b
    then eval ctx e1
    else eval ctx e2
if_ _ _ = error "illegal function call"

not' :: Context -> [SExpr] -> IO Value
not' ctx [SIdentifier "not", e] = do
  (VBool b) <- eval ctx e
  pure $ VBool (not b)
not' _ _ = error "illegal function call"

primDlopen :: Context -> [SExpr] -> IO Value
primDlopen ctx [SIdentifier "dlopen", t] = do
  (VString str) <- eval ctx t
  result <- dlopen str [RTLD_LAZY]
  pure (VPtr result)
primDlopen _ _ = error "illegal function call"

toRetType :: String -> Ret
toRetType = \case
  "string" -> TString
  "int"    -> TInteger
  "bool"   -> TBool
  _        -> error "invalid type"

primDlsym :: Context -> [SExpr] -> IO Value
primDlsym ctx [SIdentifier "dlsym", t, e0, e1] = do
  (VPtr dl) <- eval ctx t
  (VString name0) <- eval ctx e0
  (VString name1) <- eval ctx e1
  funPtr <- dlsym dl name1
  pure (VFunPtr funPtr (toRetType name0))
primDlsym _ _ = error "illegal function call"

primitives :: [(Text, Value)]
primitives =
  [ ("+", VPrim add)
  , ("-", VPrim sub)
  , ("*", VPrim mul)
  , ("=", VPrim eql)
  , ("<", VPrim ltn)
  , ("<=", VPrim lte)
  , (">", VPrim gtn)
  , (">=", VPrim gte)
  , ("define", VPrim def)
  , ("print", VPrim print)
  , ("lambda", VPrim lambda)
  , ("if", VPrim if_)
  , ("not", VPrim not')
  , ("dlopen", VPrim primDlopen)
  , ("dlsym", VPrim primDlsym)
  ]
