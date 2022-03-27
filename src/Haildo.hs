module Haildo 
  ( haildo
  ) where

import Data.Text (pack)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Syntax.Parser
import Evaluator
import System.Console.Haskeline

readFileAndEval :: FilePath -> IO ()
readFileAndEval filename = do
  output <- TIO.readFile filename
  
  either putStrLn
         (print . last . snd . evalList [])
         (parseSExpr output)

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

  if args /= []
     then readFileAndEval (head args)
     else repl
