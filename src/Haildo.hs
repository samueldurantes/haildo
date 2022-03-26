module Haildo 
  ( haildo
  ) where

import Data.Text (pack)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Syntax.Parser
import Evaluator
import System.Console.Haskeline

f :: FilePath -> IO ()
f filename = do
  output <- TIO.readFile filename

  case parseSExpr output of
    Left error -> putStrLn error
    Right result -> do
      print $ last $ snd $ evalList [] result

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

haildo :: IO ()
haildo = do
  args <- getArgs

  case args of
    [] -> repl
    [file] -> f file 
    _ -> putStrLn "--help"
