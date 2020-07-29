module Main where

import System.Environment

import Preprocessor
import Parser (runParser, tokenStreamParser)
import IR (interpretTokens)
import VM (vmInit, vmRun)

main :: IO ()
main = do 
    source <- readFile =<< head <$> getArgs
    case runParser tokenStreamParser (prepareSource source) of
        Just ("", tokens) -> 
            vmRun . vmInit $ interpretTokens tokens 0
        _ -> 
            print "Error Parsing Tokens"
