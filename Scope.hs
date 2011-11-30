module Scope where
import SymbolTypes

data SCOPE_TYPE = L_PROG
                | L_FUN FUN_NAME TACK_TYPE
                | L_BLOCK
                | L_RECORD
                | L_FORSTMT String
                | L_RECORDLIT

type FUN_NAME = String
