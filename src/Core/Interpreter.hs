module Core.Interpreter
  ( eval
  ) where

import Control.Monad (foldM)
import Data.Text (unpack, Text)
import Data.IORef (modifyIORef, readIORef)
import Syntax.Tree (SExpr (..), Context, Value (..))

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
