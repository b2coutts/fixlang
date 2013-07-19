-- module defining the datatypes used in the interpreter
module Dtypes
( Token(..)
, Value(..)
, Scope(..)
, getval
, setval ) where

import Data.Map as Map


-- datatype for parsed tokens from an FL program
data Token = Strtok String | Inttok Int | Arg Int | Dotarg Int | Name String |
             Dotname String | Lmbtok Int [String] [Token] | Apply | Punc Char
                deriving (Show, Eq)


-- datatype for evaluated values in an FL program
data Value = Strval String | Intval Int | Lmbval Int Scope [Token] |
             Empty | Cons Value Value | Func Int ([Value] -> Value)

instance Show Value where
    show (Strval x)         = show x
    show (Intval x)         = show x
    show (Lmbval i s t)     = "Lmb" ++ show i ++ show t
    show Empty              = "[]"
    show (Cons a b)         = show a ++ ":" ++ show b
    show (Func i _)         = "Func" ++ show i


-- datatype for storing a symbol table and an argument list
data Scope = Scope (Map String Value) [Value]
                deriving Show

getval                          :: Scope -> Either String Int -> Value
getval (Scope s _) (Left x)     = case Map.lookup x s of
    Nothing         -> error $ "Variable '" ++ x ++ "' is not in scope!"
    Just v          -> v
getval (Scope _ s) (Right x)
    | length s > x  = s !! x
    | otherwise     = error $ "Argument '" ++ show x ++ "' is not in scope!"

setval                          :: String -> Value -> Scope -> Scope
setval s v (Scope m l)          = Scope (Map.insert s v m) l
