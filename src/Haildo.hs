module Haildo 
  ( haildo
  ) where

import Evaluator 
import Syntax.Parser (parseSExpr)
import System.Environment (getArgs)

import qualified Data.Text.IO as TIO
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import Data.Foldable (traverse_)

haildo :: IO ()
haildo = do
  args <- getArgs
  input <- TIO.readFile (head args)
  
  case parseSExpr input of
    Left error -> putStrLn error
    Right output -> do
      _ <- State.evalStateT (traverse_ eval output) (Context Map.empty [])
      pure ()
