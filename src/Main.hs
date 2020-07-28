module Main where

data Token
    = Plus
    | Minus
    | Shl
    | Shr
    | Read
    | Write
    | Loop [Token]
    deriving (Show, Eq)

type ParseResult a = Maybe (String, a)

newtype Parser a =
    { runParser :: String -> ParseResult a
    }

charParser :: Parser Char
charParser = Parser $ 
    \input 
        | 

tokenize :: Parser [Token]
tokenize = undefined

main :: IO ()
main = undefined
