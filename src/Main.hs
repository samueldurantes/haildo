module Main where

import Core.Interpreter (eval)
import Core.Primitives (primitives)
import Data.Foldable (traverse_)
import Data.IORef (newIORef)
import Syntax.Parser (parseSExpr)
import System.Environment (getArgs)

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  input <- TIO.readFile (head args)
  case parseSExpr input of
    Left err -> error err
    Right xs -> do
      ctx <- newIORef primitives
      traverse_ (eval ctx) xs
