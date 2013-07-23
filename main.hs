-- evaluate a FL program from STDIN
import Eval
import Parse
import Dtypes

import System.Environment
import System.IO
import Control.Applicative

-- function that, given a filename, returns a token list for the file
getTokens                       :: String -> IO [Token]
getTokens file                  = do
    code <- readFile file
    let (files, tokens) = parse code

    deps <- concat <$> mapM getTokens files

    return $ deps ++ tokens

-- function to take a list of FL tokens, evaluate it, and print the result
evalPrint = putStrLn . show . evaluate

main = do
    {-
    args <- getArgs
    input <- getContents

    libs <- concat <$> mapM readFile args
    evalPrint $ libs ++ input
    -}

    args <- getArgs
    let file = if null args then error "Usage: $0 filename" else head args
    tokens <- getTokens file
    --putStrLn $ show tokens
    evalPrint tokens
