module Main where

import Preprocessor
import Parser (runParser, tokenStreamParser)
import IR (interpretTokens)
import VM

main :: IO ()
main = do 
    file <- readFile "test_files/hello.bf"
    case runParser tokenStreamParser (prepareSource file) of
        Just ("", tokens) -> 
            print $ interpretTokens tokens
        _ -> 
            print "Error Parsing Tokens"
