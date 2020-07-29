module Main where

import Preprocessor
import Parser (runParser, tokenStreamParser)
import IR (interpretTokens)
import VM (vmInit, vmRun)

main :: IO ()
main = do 
    file <- readFile "test_files/hello.bf"
    case runParser tokenStreamParser (prepareSource file) of
        Just ("", tokens) -> 
            vmRun $ vmInit $ interpretTokens tokens 0
        _ -> 
            print "Error Parsing Tokens"
