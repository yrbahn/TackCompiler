module Main where
import TackParser
import PrettyPrint

main = do
    inStr <- getContents
    case parse inStr of
        Left msg ->  putStrLn msg
        Right ast -> putStrLn $ ppAST ast
