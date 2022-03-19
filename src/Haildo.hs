module Haildo 
  ( repl 
  ) where

import Data.Text (pack)
import Syntax.Parser
import Evaluator
import System.Console.Haskeline

repl :: IO ()
repl = runInputT defaultSettings (loop [])
  where
    loop :: Context -> InputT IO ()
    loop ctx = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input -> do
          case parseSExpr (pack input) of
            Left error -> outputStrLn error
            Right result -> do
              let (ctx', v) = head (map (eval ctx) result)
              outputStrLn $ show v
              loop ctx'
