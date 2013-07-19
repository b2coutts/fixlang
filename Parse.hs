-- module to parse NL code
module Parse
( Token(..)
, parse
) where

data Token = Strtok String | Inttok Int | Arg Int | Dotarg Int | Name String |
             Dotname String | Lmbtok Int [String] [Token] | Apply | Punc Char
                deriving (Show, Eq)

-- list of characters that can begin a variable name
beginList = ['a'..'z'] ++ ['A'..'Z'] ++ "_+*/-=><"

-- list of characters that can be in the body of a variable name
bodyList = beginList ++ ['0'..'9']

-- parse an NL string from the beginning of an input string
parseStr                :: String -> [Token]
parseStr []             = error "EOF encountered during string"
parseStr ('"':xs)       = Strtok "":tkz xs
parseStr ('\\':x:xs)
    | elem x "\\\""     = Strtok (x:y):ys
    | x == 'n'          = Strtok ('\n':y):ys
    | x == 't'          = Strtok ('\t':y):ys
    | otherwise         = error $ "Bad string escape sequence '\\" ++[x]++ "'."
    where (Strtok y:ys) = parseStr xs
parseStr (x:xs)         = let (Strtok y:ys) = parseStr xs in Strtok (x:y):ys

-- parse an Int from the beginning of a string
parseInt                :: String -> (Int, String)
parseInt xs             = let (a, b) = span (`elem` ['0'..'9']) xs in
    (foldr (\x y -> (fromEnum x - 48) + 10*y) 0 $ reverse a, b)

-- convert a string into a list of tokens
tkz                     :: String -> [Token]
tkz []                  = []
tkz (';':xs)            = tkz $ tail $ takeWhile (/= '\n') xs
tkz ('"':xs)            = parseStr xs
tkz ('.':x:xs)
    | elem x ['0'..'9'] = case b of
        ('.':bs)        -> Dotarg a:tkz bs
        bs              -> Arg a:tkz bs
    | otherwise         = error "Dot found that doesn't begin an Arg"
    where (a, b)        = parseInt (x:xs)
tkz (x:xs)
    | elem x " \t\n"    = tkz xs
    | elem x "(){},$" = Punc x:tkz xs
    | elem x ['0'..'9'] = let (a, b) = parseInt (x:xs) in Inttok a:tkz b
    | elem x beginList  = let (n,r) = span (`elem` bodyList) xs in case r of
        ('.':bs)        -> Dotname (x:n):tkz bs
        bs              -> Name (x:n):tkz bs
    | otherwise         = error $ "Invalid character '" ++ [x] ++ "' parsed"

-- helper function to make recursion in pproc easier
appto           :: [Token] -> ([Token], [Token]) -> ([Token], [Token])
appto t (a, b)  = (t ++ a, b)

-- given a token list, post-processed by pproc, return its largest non-nested
-- arg, and a list containing all names referenced by it
-- Note: this may return some names that needn't be in the lambda's scope
infer                   :: [Token] -> (Int, [String])
infer []                = (0, [])
infer (Arg x:xs)        = let (a, b) = infer xs in (max a x, b)
infer (Name x:xs)       = let (a, b) = infer xs in (a, x:b)
infer (Lmbtok _ x _:xs) = let (a, b) = infer xs in (a, x ++ b)
infer (_:xs)            = infer xs

-- post-process a token list
pproc                           :: [Token] -> ([Token], [Token])
pproc []                        = ([], [])
pproc (Punc ',':xs)             = pproc xs
pproc (Punc '$':xs)             = appto [Apply] $ pproc xs
pproc (Punc '{':Inttok i:xs)    = let (a, b)    = pproc xs
                                      (_, ns)   = infer a in
    appto [(Lmbtok i ns a)] (pproc b)
pproc (Punc '{':_)              = error "Usage: {Int Expression}"
pproc (Punc '}':xs)             = ([], xs)
pproc (Punc '(':xs)             = let (a, b)    = pproc xs
                                      (i, ns)   = infer a in
    appto [(Lmbtok i ns a)] (pproc b)
pproc (Punc ')':xs)             = ([], xs)
pproc (x:xs)                    = let (a, b) = pproc xs in (x:a, b)

parse :: String -> [Token]
parse = fst . pproc . tkz

-- tests
test1 = parse "+ 1 ${2, * .1 .2} 5 4"
test2 = parse "{2, + ${1, .1} .2 5}"
test3 = parse "(+ .1 .2)"
test4 = parse "let foldr (if .3 .1  car .3 .0 .1. .2 cdr .3 else .2 ) let terse (if .3 .1 car .3 .0 .1. .2 cdr .3 .2) terse +. 0 cons 1 cons 2 cons 3 empty"