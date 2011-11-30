module Main where
import TackParser
import PrintSymbolTable
import SymbolTable
import Control.Monad
import IRTranslator
import IR

main = do
    inStr <- getContents
    case parse inStr of
        Left msg ->  putStrLn msg
        Right ast -> 
          do
            ir <- transProg 0 empty ast
            putStrLn $ show ir
