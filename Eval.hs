import Data.Map as Map

data Value = Strval String | Intval Int | Lmbval Int Scope [Token] |
             Empty | Cons Value Value | Func Int ([Value] -> Value)
                deriving Show

data Scope = Scope (Map String Value) [Value]
                deriving Show

lookup                          :: Either String Int -> Scope -> Value
lookup (Scope s _) (Left x)     = Map.lookup x s
lookup (Scope _ s) (Right x)    = s !! x

setval                          :: String -> Value -> Scope -> Scope
setval s v (Scope m l)          = Scope (Map.insert s v m) l

-- helper function; evaluate n values from a list of tokens
neval       :: Int -> Scope -> [Token] -> ([Value], [Token])
neval 0 _ ts  = ([], ts)
neval n s ts  = let (v,t) = eval s ts in let (a,b) = neval (n-1) t in (v:a, b)

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
eval s (Arg x:xs)                   = apply (lookup s Right x) xs
eval s (Dotarg x:xs)                = (lookup s $ Right x, xs)
eval s (Name x:xs)                  = apply (lookup s Left x) xs
eval s (Dotname x:xs)               = (lookup s $ Left x, xs)
eval (Scope a b) (Lmbtok i l t:xs)  = let f = (\a b -> a `elem` l) in
    (Lmbval i (Scope (Map.filterWithKey f a) b) t, xs)
eval s (Apply:xs)                   = let (a,b) = eval s xs in apply a s b
