{-# LANGUAGE DeriveFunctor #-}
module Parser
( Token (..)
, TokenStream
, ParseResult
, Parser (..)
, ParseError (..)
, tokenStreamParser
, tokenParser
)
where

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

type ParseResult a = Either ParseError (String, a)

data ParseError = ParseError
    { message :: String
    }
    deriving (Show)

parseError :: String -> ParseResult a
parseError s =
    Left $ ParseError s

newtype Parser a = Parser
    { runParser :: String -> ParseResult a
    }
    deriving (Functor)

instance Applicative Parser where
    pure x = Parser $ \input -> Right (input, x)
    Parser p1 <*> Parser p2 =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', p) <- p2 input'
            Right (input'', f p)

instance Alternative Parser where
    empty = Parser $ const $ Left (ParseError "")
    Parser p1 <|> Parser p2 =
        Parser $ \input ->
            case (p1 input, p2 input) of
                (Right a, _) -> Right a
                (Left _, Right a) -> Right a
                (Left _, Left b) -> Left b

tokenStreamParser :: Parser TokenStream
tokenStreamParser =
    paddedBy (many meaninglessChars) tokenParser

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
    Loop <$> (charParser '[' *> many meaninglessChars *> tokenStreamParser <* many meaninglessChars <* charParser ']')

charParser :: Char -> Parser Char
charParser c =
    Parser f
    where
        f (first:rest)
            | first == c = Right (rest, c)
            | otherwise = parseError $ "Expected " ++ [c] ++ " but found " ++ [first]
        f [] = parseError $ "Expected " ++ [c] ++ " but found EOF"

meaninglessChars :: Parser Char
meaninglessChars =
    Parser f
    where
        f (first:rest)
            | first `elem` "+-><.,[]" = parseError ""
            | otherwise = Right (rest, first)
        f [] = parseError ""

paddedBy :: Parser a -> Parser b -> Parser [b]
paddedBy p1 p2 =
    optional p1 *> many (p2 <* p1) <* optional p1