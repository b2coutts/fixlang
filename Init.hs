-- module defining the initial symbol table, as well as Value and Scope
module Init
( Value(..)
, Scope(..)
, getval
, setval
, initScope
) where

import Parse
import Data.Map as Map

data Value = Strval String | Intval Int | Lmbval Int Scope [Token] |
             Empty | Cons Value Value | Func Int ([Value] -> Value)

instance Show Value where
    show (Strval x)         = show x
    show (Intval x)         = show x
    show (Lmbval i s t)     = "Lmb" ++ show i ++ show t
    show Empty              = "[]"
    show (Cons a b)         = show a ++ ":" ++ show b
    show (Func i _)         = "Func" ++ show i


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


-- helper function for composing Funcs
int2     :: (Int -> Int -> Int) -> Value
int2 f   = Func 2 (\x -> case x of
    [Intval a, Intval b]    -> Intval $ f a b
    _                       -> error "ERROR: Function must take 2 Ints as args")

initTable = [
    ("+",       int2 (+)),
    ("-",       int2 (-)),
    ("*",       int2 (*)),
    ("/",       int2 quot),
    ("%",       int2 mod),

    ("if",      Func 3 myif),
    ("else",    Func 1 (\[x] -> x)),

    ("empty",   Empty),
    ("cons",    Func 2 (\[a,b] -> Cons a b)),
    ("car",     Func 1 (\[Cons a _] -> a)),
    ("cdr",     Func 1 (\[Cons _ b] -> b))
    ]


myif                        :: [Value] -> Value
myif [(Intval 0), _, v]     = v
myif [(Strval ""), _, v]    = v
myif [Empty, _, v]          = v
myif [_, v, _]              = v
myif _                      = error "Usage: if Value Value Value"

initScope = Scope (Map.fromList initTable) []
