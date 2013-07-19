-- module to parse NL code
module Parse ( parse ) where

import Dtypes

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
tkz (',':xs)            = Name "cons":tkz xs
tkz ('<':xs)            = Name "car":tkz xs
tkz ('>':xs)            = Name "cdr":tkz xs
tkz (';':xs)            = Name "empty":tkz xs
tkz (':':xs)            = Punc ':':tkz xs
tkz ('?':xs)            = Name "if":tkz xs
tkz ('#':xs)            = tkz $ tail $ dropWhile (/= '\n') xs
tkz ('"':xs)            = parseStr xs
tkz ('.':x:xs)
    | elem x ['0'..'9'] = case b of
        ('.':bs)        -> Dotarg a:tkz bs
        bs              -> Arg a:tkz bs
    | otherwise         = error "Dot found that doesn't begin an Arg"
    where (a, b)        = parseInt (x:xs)
tkz (x:xs)
    | elem x " \t\n"    = tkz xs
    | elem x "()[]{}$"  = Punc x:tkz xs
    | elem x ['0'..'9'] = let (a, b) = parseInt (x:xs) in
        if null b || head b /= '.'  then Inttok a:tkz b
                                    else Dotint a:tkz (tail b)
    | elem x beginList  = let (n,r) = span (`elem` bodyList) xs in case r of
        ('.':bs)        -> Dotname (x:n):tkz bs
        bs              -> Name (x:n):tkz bs
    | otherwise         = error $ "Invalid character '" ++ [x] ++ "' parsed"

-- find the largest arg of a token list, and a list of all names referenced
infer                   :: [Token] -> (Int, [String])
infer []                = (0, [])
infer (Arg x:xs)        = let (a, b) = infer xs in (max a x, b)
infer (Dotarg x:xs)     = let (a, b) = infer xs in (max a x, b)
infer (Name x:xs)       = let (a, b) = infer xs in (a, x:b)
infer (Dotname x:xs)    = let (a, b) = infer xs in (a, x:b)
infer (Lmbtok _ x _:xs) = let (a, b) = infer xs in (a, x ++ b)
infer (_:xs)            = infer xs

-- helper function to make recursion in pproc easier
appto           :: [Token] -> ([Token], [Token]) -> ([Token], [Token])
appto t (a, b)  = (t ++ a, b)

-- swap Ints with Args
flipDot             :: Token -> Token
flipDot (Arg x)     = Inttok x
flipDot (Inttok x)  = Arg x
flipDot (Dotarg x)  = Dotint x
flipDot (Dotint x)  = Dotarg x
flipDot x           = x

-- post-process a token list
pproc                           :: [Token] -> ([Token], [Token])
pproc []                        = ([], [])
pproc (Punc ',':xs)             = pproc xs
pproc (x:Punc ':':xs)           = pproc $ Name "let":x:xs
pproc (Punc ':':xs)             = error "Invalid use of ':'"
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
pproc (Punc '[':xs)             = let (a, b)    = pproc xs
                                      c         = map flipDot a
                                      (i, ns)   = infer c in
    appto [(Lmbtok i ns c)] (pproc b)
pproc (Punc ']':xs)             = ([], xs)
pproc (x:xs)                    = let (a, b) = pproc xs in (x:a, b)

parse :: String -> [Token]
parse = fst . pproc . tkz
