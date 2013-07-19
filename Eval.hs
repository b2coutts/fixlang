import Data.Map as Map
import Data.Maybe
import Parse

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
getval (Scope s _) (Left x)     = fromJust $ Map.lookup x s
getval (Scope _ s) (Right x)    = s !! x

setval                          :: String -> Value -> Scope -> Scope
setval s v (Scope m l)          = Scope (Map.insert s v m) l


-- Define the initial symbol table
-- helper functions for composing Funcs
int2     :: (Int -> Int -> Int) -> Value
int2 f   = Func 2 (\x -> case x of
    [Intval a, Intval b]    -> Intval $ f a b
    _                       -> error "ERROR: Function must take 2 Ints as args")

initScope = flip Scope [] $ fromList [
    ("+",       int2 (+)),
    ("-",       int2 (-)),
    ("*",       int2 (*)),
    ("/",       int2 quot),
    ("%",       int2 mod),

    ("if",      Func 3 myif)
    ]

myif                        :: [Value] -> Value
myif [(Intval 0), _, v]     = v
myif [(Strval ""), _, v]    = v
myif [Empty, _, v]          = v
myif [_, v, _]              = v
myif _                      = error "Usage: if Value Value Value"


-- helper function; evaluate n values from a list of tokens
neval       :: Int -> Scope -> [Token] -> ([Value], [Token])
neval 0 _ ts  = ([], ts)
neval n s ts  = let (v,t) = eval s ts in let (a,b) = neval (n-1) s t in (v:a, b)

-- evaluate a value in function application context
apply                           :: Value -> Scope -> [Token] -> (Value, [Token])
apply (Func i f) s xs               = let (vs, ts) = neval i s xs in (f vs, ts)
apply (Lmbval i (Scope m _) t) s xs = let (vs, ts) = neval i s xs in
    (fst $ eval (Scope m vs) t, ts)
apply v _ xs                        = (v, xs)

-- main evaluation function; evaluates a list of tokens
eval                                :: Scope -> [Token] -> (Value, [Token])
eval s@(Scope a b) (Name "let":Name n:xs)  = let (v, t) = eval s xs in
    eval (Scope (Map.insert n v a) b) t
eval s (Strtok x:xs)                = (Strval x, xs)
eval s (Inttok x:xs)                = (Intval x, xs)
eval s (Arg x:xs)                   = apply (getval s $ Right x) s xs
eval s (Dotarg x:xs)                = (getval s $ Right x, xs)
eval s (Name x:xs)                  = apply (getval s $ Left x) s xs
eval s (Dotname x:xs)               = (getval s $ Left x, xs)
eval (Scope a b) (Lmbtok i l t:xs)  = let f = (\a b -> a `elem` l) in
    (Lmbval i (Scope (Map.filterWithKey f a) b) t, xs)
eval s (Apply:xs)                   = let (a,b) = eval s xs in apply a s b

evaluate                            :: String -> Value
evaluate = fst . eval initScope . parse
