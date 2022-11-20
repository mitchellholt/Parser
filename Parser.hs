{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parser where

import Result
import Control.Applicative
import Control.Monad

newtype Parser a = P (String -> Result String (a, String))

parse :: Parser a -> String -> Result String (a, String)
parse (P f) = f

class Parseable a where
    tryparse :: Parser a

instance Monad Parser where
    p >>= f =
        let
            g = \str -> case parse p str of
                (Ok (b, str')) -> parse (f b) str'
                (Error a)      -> Error a
        in
            P g

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = P (\str -> Ok (a, str))
    pf <*> pa = do
        f <- pf
        f <$> pa

instance Functor Parser where
    fmap f pa = do
        a <- pa
        return (f a)

instance Alternative Parser where
    empty = P (const (Error []))
    pa <|> pb =
        let
            g str = case parse pa str of
                Error m -> case parse pb str of
                    Error n -> Error (m ++ n)
                    mx      -> mx
                my          -> my
        in
            P g

instance MonadFail Parser where
    fail str = P (\_ -> Error str)


char :: Parser Char
char =
    let
        g str = case str of
            [] -> Error "could not parse any characters"
            (c:cs) -> Ok (c, cs)
    in
        P g


while :: (Char -> Bool) -> Parser String
while p =
    let
        g str = case str of
            [] -> Ok ("", "")
            (c:cs)
                | p c -> case parse (while p) cs of
                    Error _         -> Ok ([c], "")
                    Ok (rest, str') -> Ok (c:rest, str')
                | otherwise -> Ok ("", c:cs)
    in
        P g


newline :: Parser ()
newline = do
    do
        a <- char
        unless (a == '\n') empty
    <|>
        fail "could not parse a new line"


spaces :: Parser ()
spaces = do
    do
        str <- while (== ' ')
        when (null str) empty
    <|>
        fail "could not parse any spaces"


whitespace :: Parser ()
whitespace = do
    _ <- while (`elem` " \n")
    return ()


exact :: String -> Parser ()
exact str = exact' str str
    where
        exact' :: String -> String -> Parser ()
        exact' [] _ = return ()
        exact' (c:cs) m = do
            a <- char
            if a == c then exact' cs m
            else
                fail ("could not parse the exact string " ++ m)


token :: String -> Parser ()
token str = do
    do
        whitespace
        _ <- exact str
        whitespace
        return ()
    <|>
        fail ("Could not parse the token " ++ str)


instance Parseable Int where
    tryparse = do
        n <- while (`elem` "0123456789")
        if null n
           then fail "could not parse an integer"
           else return (read n)


instance Parseable Integer where
    tryparse = do
        n <- while (`elem` "0123456789")
        if null n
           then fail "could not parse an integer"
           else return (read n)


list :: Parseable a => Parser [a]
list = do
    do
        x <- tryparse
        token ","
        xs <- list
        if null xs
           then fail "Could not parse list, unexpected commad after last element"
           else return (x:xs)
    <|> (do 
        x <- tryparse
        return [x]
    <|> return [])


instance (Parseable a) => Parseable [a] where
    tryparse = do
        token "["
        xs <- list
        token "]"
        return xs
