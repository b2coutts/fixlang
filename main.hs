-- evaluate a FL program from STDIN
import Eval
import Parse

main = do
    input <- getContents

    putStrLn $ show $ evaluate $ input
    -- putStrLn $ show $ parse $ input
