-- evaluate a FL program from STDIN
import Eval

main = do
    input <- getContents

    putStrLn $ show $ evaluate $ input
