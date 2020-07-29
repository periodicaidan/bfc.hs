{-# LANGUAGE DeriveFunctor #-}
module Parser where

import Control.Applicative

data Token
    = Plus              -- (+)
    | Minus             -- (-)
    | Shl               -- (<)
    | Shr               -- (>)
    | Read              -- (,)
    | Write             -- (.)
    | Loop TokenStream  -- ([ ... ])
    deriving (Show, Eq)

type TokenStream = [Token]

type ParseResult a = Maybe (String, a)

newtype Parser a = Parser
    { runParser :: String -> ParseResult a
    }
    deriving (Functor)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    Parser p1 <*> Parser p2 = 
        Parser $ \input -> do 
            (input', f) <- p1 input 
            (input'', p) <- p2 input'
            Just (input'', f p)

instance Alternative Parser where 
    empty = Parser $ const Nothing
    Parser p1 <|> Parser p2 = 
        Parser $ \input -> p1 input <|> p2 input

tokenStreamParser :: Parser TokenStream
tokenStreamParser = 
    many tokenParser

tokenParser :: Parser Token 
tokenParser = 
    plusTokenParser 
    <|> minusTokenParser
    <|> shlTokenParser
    <|> shrTokenParser
    <|> writeTokenParser
    <|> readTokenParser
    <|> loopTokenParser

singleTokenParser :: Char -> Token -> Parser Token 
singleTokenParser c t = 
    t <$ charParser c 

plusTokenParser :: Parser Token 
plusTokenParser = 
    singleTokenParser '+' Plus 

minusTokenParser :: Parser Token
minusTokenParser = 
    singleTokenParser '-' Minus

shlTokenParser :: Parser Token
shlTokenParser = 
    singleTokenParser '<' Shl 

shrTokenParser :: Parser Token
shrTokenParser = 
    singleTokenParser '>' Shr

readTokenParser :: Parser Token 
readTokenParser = 
    singleTokenParser ',' Read

writeTokenParser :: Parser Token 
writeTokenParser = 
    singleTokenParser '.' Write

loopTokenParser :: Parser Token 
loopTokenParser =
    Loop <$> (charParser '[' *> tokenStreamParser <* charParser ']')

charParser :: Char -> Parser Char
charParser c = 
    Parser f
    where 
        f (first:rest) 
            | first == c = Just (rest, c)
            | otherwise = Nothing
        f [] = Nothing