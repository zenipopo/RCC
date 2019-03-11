module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import LexC
import ParC
import PrintC
import AbsC
import ErrM
import Translate

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

-- runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

-- run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
           -- Ok  tree -> do putStrLn "\nParse Successful!"
           --                showTree v tree
           Ok  tree -> showTree v (transProgram tree)

-- showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree :: Int -> Program -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 2 pProgram
            "-s":fs -> mapM_ (runFile 0 pProgram) fs
            fs -> mapM_ (runFile 2 pProgram) fs
