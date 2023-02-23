module Core.Interpreter
  ( eval
  ) where

import Control.Monad (foldM)
import Data.Text (unpack, Text)
import Syntax.Tree (SExpr (..), Context(..), Value (..), Ret(..))
import Data.IORef (readIORef)
import Core.Marshalling (marshal)
import Foreign.LibFFI (callFFI)
import Foreign.LibFFI.Types ( retInt64 )

findVariable :: Text -> Context -> IO Value
findVariable name ctx =
  case lookup name ctx.locals of
    Nothing -> do
      ctx' <- readIORef ctx.globals
      case lookup name ctx' of
        Nothing -> error $ "Variable \"" ++ unpack name ++ "\" not found!"
        Just va -> pure va
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
          VFunPtr fPTr ret -> do
            args <- traverse (eval ctx) ys
            let b = map marshal args
            case ret of
              TInteger -> do
                i <- callFFI fPTr retInt64 b
                pure $ VInteger (fromIntegral i)
              _ -> error "not implemented yet"
          _ -> error "illegal function call"

apply :: Context -> Value -> SExpr -> IO Value
apply ctx fun arg = do
  a <- eval ctx arg
  case fun of
    VClosure ctx' n e -> do
      let ct = Context { globals = ctx'.globals, locals = (n, a) : ctx'.locals }
      eval ct e
    _ -> error "illegal function call"
