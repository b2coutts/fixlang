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
    _                     -> error "ERROR: Arith function needs 2 Ints as args")

-- helper function for returning booleans
boolToVal           :: Bool -> Value
boolToVal x         = if x then Intval 1 else Intval 0

initTable = [
    ("+",       int2 (+)),
    ("-",       int2 (-)),
    ("*",       int2 (*)),
    ("/",       int2 quot),
    ("%",       int2 mod),

    ("if",      Func 3 myif),
    ("else",    Func 1 (\[x] -> x)),
    ("gt",      Func 2 gt),
    ("lt",      Func 2 lt),
    ("eq",      Func 2 eq),

    ("empty",   Empty),
    ("cons",    Func 2 (\[a,b] -> Cons a b)),
    ("car",     Func 1 mycar),
    ("cdr",     Func 1 mycdr),

    ("substr",  Func 3 mysubstr),
    ("strcat",  Func 2 mystrcat)
    ]

gt [(Intval a), (Intval b)]  = boolToVal $ a > b
gt [(Strval a), (Strval b)]  = boolToVal $ a > b
gt _                        = error "Usage: gt Int Int or gt String String"

lt [(Intval a), (Intval b)]  = boolToVal $ a < b
lt [(Strval a), (Strval b)]  = boolToVal $ a < b
lt _                        = error "Usage: lt Int Int or lt String String"

eq [(Intval a), (Intval b)]  = boolToVal $ a == b
eq [(Strval a), (Strval b)]  = boolToVal $ a == b
eq _                        = error "Usage: eq Int Int or eq String String"

mycar [Empty]               = error "car called on empty list"
mycar [Strval ""]           = error "car called on empty string"
mycar [Cons a _]            = a
mycar [Strval (x:_)]        = Strval [x]
mycar _                     = error "Usage: car Cons or car String"

mycdr [Empty]               = error "cdr called on empty list"
mycdr [Strval ""]           = error "cdr called on empty string"
mycdr [Cons _ b]            = b
mycdr [Strval (_:xs)]       = Strval xs
mycdr _                     = error "Usage: cdr Cons or cdr String"


myif [(Intval 0), _, v]     = v
myif [(Strval ""), _, v]    = v
myif [Empty, _, v]          = v
myif [_, v, _]              = v
myif _                      = error "Usage: if Value Value Value"

mysubstr [Strval s, Intval a, Intval b]
    | length s <= b     = error "ERROR: third arg to substr out of range"
    | b < a             = error "ERROR: 3rd arg of substr less than 2nd arg"
    | otherwise         = Strval $ take (b-a) $ drop a s
mysubstr _                              = error "Usage: substr String Int Int"

mystrcat [Strval a, Strval b]           = Strval $ a ++ b
mystrcat _                              = error "Usage: strcat String String"

initScope = Scope (Map.fromList initTable) []
