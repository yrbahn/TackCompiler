module SymbolTable where
import SymbolTypes
import Scope
import Data.Maybe
import AST
import List(sort)

-- define Symbol Table
data SYM_TABLE = Sym_Table (SCOPE_TYPE, Int, Int, [(SYMBOL, SYMBOL_VALUE)])
type ST = [SYM_TABLE]

instance Show SYM_TABLE where
  show (Sym_Table (L_PROG, _, _, l))       = "Program {" ++ toString (sort $ map fst l) ++ "}"
  show (Sym_Table (L_FUN n  _, _, _, l))   = "FunDef " ++ n ++" {" ++ toString ( sort $ map fst l) ++ "}"
  show (Sym_Table (L_RECORD , _, _, l))    = "RecordType {" ++ toString (sort $ map fst l) ++ "}"
  show (Sym_Table (L_RECORDLIT , _, _, l)) = "RecordLit {" ++ toString (sort $ map fst l) ++ "}"
  show (Sym_Table (L_FORSTMT n, _, _, l))  = "ForStmt " ++ n ++ " {" ++ toString (sort $ map fst l) ++ "}"
  show (Sym_Table (L_BLOCK , _, _, l))     = "BlockStmt {" ++ toString (sort $ map fst l) ++ "}"

toString :: [String] -> String
toString (x:[]) = x 
toString (x:xs) = x ++ ", " ++ toString xs
toString []    = ""   
    
countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

-- Empty Symbol Table
empty :: ST
empty = []

-- create a new scope
new_scope :: SCOPE_TYPE -> ST -> ST
new_scope scopeType st = Sym_Table(scopeType, 0,0,[]):st

-- insert a symbol into the symbol table
insert :: ST -> SYMBOL_DESC -> ST
insert [] d = error "Symbol table error : insertion before defining scope."
insert ((Sym_Table(scopeT, nLocal, nArg, lSym)):rest) (ARGUMENT(str, t)) =
  case in_index_list str lSym of
    True  -> error ("Symbol table error : " ++ str ++ " is already defined")
    False -> Sym_Table(scopeT, nLocal, nArg+1, (str, VAR_ATTR(-1*nArg,t)):lSym):rest
insert ((Sym_Table(scopeT, nLocal, nArg, lSym)):rest) (VARIABLE (str, t))  =
  case in_index_list str lSym  of 
    True  -> error ("Symbol table error :" ++ str ++ " is already defined")
    False -> Sym_Table(scopeT, nLocal+1, nArg, (str, VAR_ATTR(nLocal+1,t)):lSym):rest
insert ((Sym_Table(scopeT, nLocal, nArg, lSym)):rest) (FUNCTION (str, argT, retT)) =
  case in_index_list str lSym of
    True  -> error ("Symbol table error : " ++ str ++ " is already defined")
    False -> Sym_Table(scopeT, nLocal, nArg, (str, FUN_ATTR("func",argT, retT)):lSym):rest
insert ((Sym_Table(scopeT, nLocal, nArg, lSym)):rest) (SUBST(str, str2, vT)) =
  case in_index_list str lSym of
    True  -> error ("Symbol table error : " ++ str ++ " is already defined")
    False -> Sym_Table(scopeT, nLocal+1, nArg, (str, SUBST_ATTR(nLocal+1, str2, vT)):lSym):rest

in_index_list :: SYMBOL -> [(SYMBOL,SYMBOL_VALUE)] -> Bool
in_index_list str [] = False
in_index_list str ((x,_):xs) | str==x  = True
                             | otherwise = in_index_list str xs

-- lookup a symbol
look_up :: ST -> SYMBOL -> SYMBOL_I_DESC
look_up s x = find 0 s where
  found level (VAR_ATTR(offset, t)) = I_VARIABLE(level, offset, t)
  found level (FUN_ATTR(label, argType, retType)) = I_FUNCTION(level, label, argType, retType)
  found level (SUBST_ATTR(offset, str, varType)) = I_SUBST(level, offset, str, varType)

  find_level ((str,v):rest) | x == str = Just v
                            | otherwise = find_level rest
  find_level [] = Nothing

  find n [] = NO_FOUND
  find n (Sym_Table(_, _, _, vs):rest) = 
    case find_level vs of 
      Just v -> found n v
      Nothing -> find (n+1) rest

-- return function ret Type
fReturn :: ST -> TACK_TYPE
fReturn (x:xs) = 
  case x of
    Sym_Table(L_FUN _ retT, _,_,_) -> retT
    _ -> fReturn xs
fReturn [] = TK_ERROR


