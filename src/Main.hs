module Main where

import System.Environment

import Parser (ParseError (..), runParser, tokenStreamParser)
import IR (interpretTokens)
import VM (vmInit, vmRun)

main :: IO ()
main = do
    source <- readFile =<< head <$> getArgs
    case runParser tokenStreamParser source of
        Right ("", tokens) ->
            vmRun . vmInit $ interpretTokens tokens 0
        Right (s, tokens) ->
            print "Failed to tokenize whole source"
        Left (ParseError msg) ->
            print msg
