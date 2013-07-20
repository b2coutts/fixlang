-- module defining the initial symbol table, as well as Value and Scope
module Init
( Value(..)
, Scope(..)
, getval
, setval
, initScope
) where

import Parse
import Dtypes
import Data.Map as Map

-- helper function for composing Funcs
int2     :: (Int -> Int -> Int) -> Value
int2 f   = Func 2 (\x -> case x of
    [Intval a, Intval b]  -> Intval $ f a b
    _                     -> Error "ERROR: Arith function needs 2 Ints as args")

-- helper function for returning booleans
boolToVal               :: Bool -> Value
boolToVal x             = if x then Intval 1 else Intval 0

-- helper function for evaluating Intvals as bools
valToBool               :: Value -> Bool
valToBool (Intval i)    = i /= 0

initTable = [
    ("+",       int2 (+)),
    ("-",       int2 (-)),
    ("*",       int2 (*)),
    ("/",       int2 quot),
    ("mod",       int2 mod),

    ("not",     Func 1 $ mynot),
    ("if",      Func 3 myif),
    ("else",    Func 1 (\[x] -> x)),
    ("gt",      Func 2 $ gt),
    ("lt",      Func 2 $ lt),
    ("eq",      Func 2 $ eq),

    ("empty",   Empty),
    ("cons",    Func 2 (\[a,b] -> Cons a b)),
    ("car",     Func 1 mycar),
    ("cdr",     Func 1 mycdr),

    ("error",   Func 1 myerror)
    ]

gt :: [Value] -> Value
gt [Error e, _]             = Error $ "gt1: " ++ e
gt [_, Error e]             = Error $ "gt2: " ++ e
gt [Intval a, Intval b]     = boolToVal $ a > b
gt [Strval a, Strval b]     = boolToVal $ a > b
gt [Cons a b, Cons c d]
    | valToBool $ gt [a, c] = boolToVal $ True
    | valToBool $ lt [a, c] = boolToVal $ False
    | otherwise             = gt [b, d]
gt [Empty, _]               = boolToVal $ False
gt [_, Empty]               = boolToVal $ False
gt _                        = Error "Usage: gt Int Int or gt String String"

lt [Error e, _]             = Error $ "lt1: " ++ e
lt [_, Error e]             = Error $ "lt2: " ++ e
lt [Intval a, Intval b]     = boolToVal $ a < b
lt [Strval a, Strval b]     = boolToVal $ a < b
lt [Cons a b, Cons c d]
    | valToBool $ lt [a, c] = boolToVal $ True
    | valToBool $ gt [a, c] = boolToVal $ False
    | otherwise             = lt [b, d]
lt _                        = Error "Usage: lt Int Int or lt String String"


eq [Error e, _, _]          = Error $ "eq1: " ++ e
eq [_, Error e, _]          = Error $ "eq2: " ++ e
eq [_, _, Error e]          = Error $ "eq3: " ++ e
eq [Intval a, Intval b]     = boolToVal $ a == b
eq [Strval a, Strval b]     = boolToVal $ a == b
eq [Cons a b, Cons c d]     = boolToVal $ valToBool (eq [a,c])
                                && valToBool (eq [b,d])
eq [Empty, Empty]           = boolToVal $ True
eq [_, Empty]               = boolToVal $ False
eq [Empty, _]               = boolToVal $ False
eq _                        = Error "Usage: eq Val Val"

mycar [Error e]             = Error $ "car1: " ++ e
mycar [Empty]               = Error "car called on empty list"
mycar [Strval ""]           = Error "car called on empty string"
mycar [Cons a _]            = a
mycar [Strval (x:_)]        = Strval [x]
mycar _                     = Error "Usage: car Cons or car String"

mycdr [Error e]             = Error $ "cdr1: " ++ e
mycdr [Empty]               = Error "cdr called on empty list"
mycdr [Strval ""]           = Error "cdr called on empty string"
mycdr [Cons _ b]            = b
mycdr [Strval (_:xs)]       = Strval xs
mycdr _                     = Error "Usage: cdr Cons or cdr String"


mynot [Error e]             = Error $ "not: " ++ e
mynot [Intval 0]            = boolToVal $ True
mynot [Strval ""]           = boolToVal $ True
mynot [Empty]               = boolToVal $ True
mynot _                     = boolToVal $ False

myif [Error e, _, _]        = Error $ "if1: " ++ e
myif [Intval 0, _, Error e] = Error $ "if3: " ++ e
myif [Strval "",_, Error e] = Error $ "if3: " ++ e
myif [Empty, _, Error e]    = Error $ "if3: " ++ e
myif [Intval 0, _, v]       = v
myif [Strval "", _, v]      = v
myif [Empty, _, v]          = v
myif [_, Error e, _]        = Error $ "if2: " ++ e
myif [_, v, _]              = v
myif _                      = Error "Usage: if Value Value Value"

myerror [Strval s]      = Error $ "PROGRAM ERROR: " ++ s
myerror _               = Error "ERROR: Usage: error String"

initScope = Scope (Map.fromList initTable) []
