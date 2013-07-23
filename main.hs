-- evaluate a FL program from STDIN
import Eval
import Parse
import Dtypes

import System.Environment
import System.IO
import Control.Applicative

-- location of FL standard library
stdLib = "/home/b2coutts/git/fixlang/lib"

-- function that, given a filename, returns a token list for the file
getTokens                       :: String -> IO [Token]
getTokens file                  = do
    code <- readFile file
    let (files, tokens) = parse code

    deps <- concat <$> mapM (getTokens . findLib) files

    return $ deps ++ tokens

-- given a string representing a library, return a filepath
findLib             :: String -> String
findLib ""          = error "ERROR: empty library name given for import"
findLib s@('/':xs)  = s
findLib s@('.':xs)  = s
findLib xs          = stdLib ++ "/" ++ xs

-- function to take a list of FL tokens, evaluate it, and print the result
evalPrint = putStrLn . show . evaluate

main = do
    args <- getArgs
    let file = if null args then error "Usage: $0 filename" else head args
    tokens <- getTokens file
    evalPrint tokens
