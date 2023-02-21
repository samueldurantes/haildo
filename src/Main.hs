module Main where

import Syntax.Parser (parseSExpr)
import Syntax.Tree (SExpr (..), Context, Value (..))
import Data.Text (unpack, Text)
import qualified Data.Text.IO as TIO
import Data.IORef
import Control.Monad (foldM)
import Data.Foldable (traverse_)
import System.Environment (getArgs)

findVariable :: Text -> Context -> IO Value
findVariable name ctx = do
  ctx' <- readIORef ctx
  case lookup name ctx' of
    Nothing ->
      error $ "Variable \"" ++ unpack name ++ "\" not found!"
    Just va -> pure va

eval :: Context -> SExpr -> IO Value
eval ctx = \case
  SInteger i -> pure $ VInteger i
  SBool b -> pure $ VBool b
  SString s -> pure $ VString s
  SIdentifier i -> findVariable i ctx
  SSExpr xs ->
    case xs of
      []       -> error "illegal function call"
      zs@(x : ys) -> do
        f <- eval ctx x
        case f of
          VClosure {} -> foldM (apply ctx) f ys
          VPrim primitiveFn -> primitiveFn ctx zs
          _ -> error "illegal function call"

apply :: Context -> Value -> SExpr -> IO Value
apply ctx fun arg = do
  a <- eval ctx arg
  case fun of
    VClosure ctx' n e -> do
      modifyIORef ctx' (\x -> (n, a) : x)
      eval ctx' e
    _ -> error "illegal function call"

isInt :: Value -> Bool
isInt (VInteger _) = True
isInt _ = False

toInt :: Value -> Integer
toInt (VInteger i) = i
toInt _ = error "NaN"

primAdd :: Context -> [SExpr] -> IO Value
primAdd ctx (SIdentifier "+" : rest) = do
  xs <- traverse (eval ctx) rest
  if all isInt xs
    then pure $ VInteger $ sum $ map toInt xs
    else error "illegal function call"
primAdd _ _ = error "illegal function call"

primDef :: Context -> [SExpr] -> IO Value
primDef ctx [SIdentifier "define", SIdentifier n, body] = do
  b <- eval ctx body
  modifyIORef ctx (\ctx' -> (n, b) : ctx')
  pure VNil
primDef _ _ = error "illegal function call"

primPrint :: Context -> [SExpr] -> IO Value
primPrint ctx (SIdentifier "print" : rest) = do
  b <- traverse (eval ctx) rest
  putStrLn $ unwords $ map show b
  pure VNil
primPrint _ _ = error "illegal function call"

initialContext :: IO Context
initialContext = newIORef
  [ ("+", VPrim primAdd)
  , ("define", VPrim primDef)
  , ("print", VPrim primPrint)
  ]

main :: IO ()
main = do
  args <- getArgs
  input <- TIO.readFile (head args)
  case parseSExpr input of
    Left err -> error err
    Right xs -> do
      ctx <- initialContext
      traverse_ (eval ctx) xs
