module Main where

import Core.Interpreter (eval)
import Core.Primitives (primitives)
import Data.Foldable (traverse_)
import Data.IORef (newIORef)
import Syntax.Parser (parseSExpr)
import Syntax.Tree (Context(..))
import System.Environment (getArgs)

import qualified Data.Text.IO as TIO

initialContext :: IO Context
initialContext = do
  globals <- newIORef primitives
  pure (Context { globals, locals = [] })

main :: IO ()
main = do
  args <- getArgs
  input <- TIO.readFile (head args)
  case parseSExpr input of
    Left err -> error err
    Right xs -> do
      ctx <- initialContext
      traverse_ (eval ctx) xs
