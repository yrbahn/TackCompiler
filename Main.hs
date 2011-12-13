module Main where
import TackParser
import SemanticAnalyzer
import SymbolTable
import IRTranslator
import IR
import CodeGen
import ASM
import Control.Monad.Trans

main = do
    inStr <- getContents
    case parse inStr of
        Left msg -> putStrLn msg
        Right ast -> 
          do
            semanticAnalyzer 0 empty ast
            ir <- transProg 0 empty ast
            (asm, _) <- runASM (codeGenProg ir) emptyASMState
            putStrLn $ show asm
