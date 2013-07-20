-- evaluate a FL program from STDIN
import Eval
import Parse

import System.Environment
import System.IO
import Control.Applicative

-- function to take a string of FL code, evaluate it, and print the result
evalPrint = putStrLn . show . evaluate

main = do
    args <- getArgs
    input <- getContents

    libs <- concat <$> mapM readFile args

    evalPrint $ libs ++ input
