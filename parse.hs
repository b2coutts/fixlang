data Token = Strtok String | Inttok Int | Arg Int | Dotarg Int | Name String |
             Dotname String | Lambda Int [Token] | Apply |
             -- the following token should NOT be returned by parse
             Punc Char
                deriving Show

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
-- TODO: could merge (name, rname) = into the earlier case line
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
    | elem x "()[]{},$" = Punc x:tkz xs
    | elem x ['0'..'9'] = let (a, b) = parseInt (x:xs) in Inttok a:tkz b
    | elem x beginList  = case rname of
        ('.':bs)        -> Dotname (x:name):tkz bs
        bs              -> Name (x:name):tkz bs
    | otherwise         = error $ "Invalid character '" ++ [x] ++ "' parsed"
    where (name, rname) = span (`elem` bodyList) xs
