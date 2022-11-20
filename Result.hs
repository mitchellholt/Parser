{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Result where

data Result a b = Error a | Ok b

instance Monad (Result a) where
    (>>=) (Error a) _ = Error a
    (>>=) (Ok b) f    = f b

instance Applicative (Result a) where
    pure = Ok
    (<*>) f x = do
        g <- f
        g <$> x

instance Functor (Result a) where
    fmap f x = do
        a <- x
        return (f a)

instance (Show a, Show b) => Show (Result a b) where
    show (Error b) = "Error: " ++ show b
    show (Ok a) = show a
