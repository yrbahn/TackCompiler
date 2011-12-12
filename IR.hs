module IR where

import SymbolTable
import SymbolTypes
import AST

type ILabel  = String
type IIndex  = Int
type IArity  = Int
type IString = String
type IId     = String
type IOp     = String

data IProg = IPROG [IFun]

instance Show IProg where
  show (IPROG funDefList) = 
    unlines $ map show funDefList

data IFun  = IFUN (ST, IString, TACK_TYPE, [IStmt]) 

instance Show IFun where
  show (IFUN(_, funId, ty, stmtList)) = 
    funId ++ " = fun " ++ show ty ++ "\n" ++ (unlines $ map (\x-> show x ++ ";") stmtList)

data IStmt = ISTMT ([ILabel], IInst) 

instance Show IStmt where
  show (ISTMT ([], inst)) = show inst 
  show (ISTMT (labels, inst)) = (unwords $ map (\x -> x ++":") labels) ++ show inst 

data IInst = ICOPY (IAddr, IAddr)
             | IPLUS  (IAddr, IAddr, IAddr)
             | IMINUS (IAddr, IAddr, IAddr)
             | ITIMES (IAddr, IAddr, IAddr)
             | IDIV   (IAddr, IAddr, IAddr)
             | IMOD   (IAddr, IAddr, IAddr)
             | IPMINUS (IAddr, IAddr )
             | ICAST (IAddr, IAddr, TACK_TYPE)
             | IUNCOND_JUMP (ILabel)
             | ITRUE_JUMP(IAddr, ILabel)
             | IFALSE_JUMP(IAddr, ILabel)
             | IEQ_JUMP (IAddr, IAddr, ILabel)
             | INEQ_JUMP (IAddr, IAddr, ILabel)
             | IGT_JUMP (IAddr, IAddr, ILabel)
             | IGE_JUMP (IAddr, IAddr, ILabel)
             | ILT_JUMP (IAddr, IAddr, ILabel)
             | ILE_JUMP (IAddr, IAddr, ILabel)
             | IPARAM (IIndex, IArity, IAddr)
             | ICALL (IId, IArity)
             | ICALLR (IAddr, IId, IArity)
             | IRETURN (Maybe IAddr)
             | IARRAY_READ(IAddr, IAddr, IAddr)
             | IARRAY_WRITE(IAddr, IAddr, IAddr)
             | IRECORD_READ(IAddr, IAddr, IId)
             | IRECORD_WRITE(IAddr, IId, IAddr)
 
instance Show IInst where
  show (ICOPY(l,r)) = show l ++ " = " ++ show r
  show (IPLUS(a,b,c)) = show a ++ " = " ++ show b ++ " + " ++ show c
  show (IMINUS(a,b,c)) = show a ++ " = " ++ show b ++ " - " ++ show c
  show (ITIMES(a,b,c)) = show a ++ " = " ++ show b ++ " * " ++ show c
  show (IDIV(a,b,c)) = show a ++ " = " ++ show b ++ " / " ++ show c
  show (IMOD(a,b,c)) = show a ++ " = " ++ show b ++ " % " ++ show c
  show (ICAST(a,b,t)) = show a ++ " = " ++ show b ++ " : " ++ show t
  show (IPMINUS(a,b)) = show a ++ " = -" ++ show b
  show (IUNCOND_JUMP(l)) = "goto " ++ l
  show (ITRUE_JUMP(a,l)) = "if " ++ show a ++ " goto " ++ l
  show (IFALSE_JUMP(a,l)) = "ifFalse " ++ show a ++ " goto " ++ l
  show (IEQ_JUMP(a,b,l))  = "if " ++ show a ++ " == " ++ show b ++ " goto " ++ l
  show (INEQ_JUMP(a,b,l))  = "if " ++ show a ++ " != " ++ show b ++ " goto " ++ l
  show (IGT_JUMP(a,b,l))  = "if " ++ show a ++ " > " ++ show b ++ " goto " ++ l
  show (IGE_JUMP(a,b,l))  = "if " ++ show a ++ " >= " ++ show b ++ " goto " ++ l
  show (ILT_JUMP(a,b,l))  = "if " ++ show a ++ " < " ++ show b ++ " goto " ++ l
  show (ILE_JUMP(a,b,l))  = "if " ++ show a ++ " <= " ++ show b ++ " goto " ++ l
  show (IPARAM(i,a,b)) = "param [" ++ show i ++ " : " ++ show a ++ "] =" ++ show b 
  show (ICALL(i,a)) = "call " ++ i ++ " : " ++ show a
  show (ICALLR(r,i,a)) = show r ++ "= call " ++ i ++ " : " ++ show a
  show (IRETURN(r)) = 
    case r of
      Just r' -> "return " ++ show r'
      Nothing -> "return"
  show (IARRAY_READ(a,b,c)) = show a ++ " = " ++ show b ++ "[" ++ show c ++ "]"
  show (IARRAY_WRITE(a,b,c)) = show a ++ "[" ++ show b ++"] = " ++ show c
  show (IRECORD_READ(a,b,i)) = show a ++ " = " ++ show b ++ "." ++ i
  show (IRECORD_WRITE(a,i,b)) = show a ++ "." ++ i ++ " = " ++ show b
       	             
data IAddr = IID String TACK_TYPE
            | IBOOL Bool
            | IINT  Int
            | ISTRING String
            | INULL
            | ISIZEOF TACK_TYPE

instance Show IAddr where
  show (IID str _) = str
  show (IBOOL b) = 
    case b of
      True  -> "true"
      False -> "false"
  show (IINT i ) = show i
  show (ISTRING str) = show str
  show INULL        = "null"
  show (ISIZEOF ty) = "sizeof(" ++ show ty ++ ")"


makeStmt :: Maybe [ILabel] -> IInst -> IStmt
makeStmt labels inst =
  case labels of 
    Just labels' -> ISTMT(labels', inst)
    Nothing     -> ISTMT([], inst)

makeStmt' ::  IInst -> IStmt
makeStmt'  inst =
  ISTMT([], inst)

isAddr :: Expr -> Bool
isAddr (StringLit{stringValue=_, exprSrcPos=_}) = True
isAddr (BoolLit{boolValue=_, exprSrcPos=_})     = True
isAddr (IntLit{intValue=_, exprSrcPos=_})       = True
isAddr (NullLit{exprSrcPos=_})                  = True
isAddr (FunId {funIdName=_, exprSrcPos=_})      = True
isAddr (VarId {varIdName=_, exprSrcPos=_})      = True
isAddr _                                        = False

getAddr :: ST -> Expr -> IAddr
getAddr st (StringLit{stringValue=s, exprSrcPos=_}) = ISTRING s
getAddr st (BoolLit{boolValue=b, exprSrcPos=_})     = IBOOL b 
getAddr st (IntLit{intValue=i, exprSrcPos=_})       = IINT i
getAddr st (NullLit{exprSrcPos=_})                  = INULL
getAddr st (FunId {funIdName=fi, exprSrcPos=_})     = IID fi TK_NULL
getAddr st (VarId {varIdName=vi, exprSrcPos=_})     = 
  case look_up st vi of 
    I_SUBST(_,_,str,ty) -> IID str ty
    I_VARIABLE(_,_,ty) -> IID vi ty
    _ -> error "no such id"

getAddr _ _                                        = INULL


getAddrType :: IAddr -> TACK_TYPE
getAddrType (IID _ t) = t
getAddrType (IBOOL _) = TK_BOOL
getAddrType (IINT _)  = TK_INT
getAddrType (ISTRING _) = TK_STRING
getAddrType INULL = TK_NULL
getAddrType (ISIZEOF _) = TK_INT
 

addLabel :: Maybe [ILabel] -> IStmt -> IStmt
addLabel labels stmt@(ISTMT(labelL ,instr))  = 
  case labels of
    Just labels' -> ISTMT(labelL++labels',instr)
    Nothing     -> stmt

addLabelToStmtList :: Maybe [ILabel] -> [IStmt] -> [IStmt]
addLabelToStmtList label []     = []
addLabelToStmtList label (x:xs) = (addLabel label x):xs

getLeftAddr :: IInst -> Maybe String
getLeftAddr inst = 
  case inst of
    ICOPY (IID var _, _)     -> Just var
    IPLUS  (IID var _, _, _) -> Just var
    IMINUS (IID var _, _, _) -> Just var
    ITIMES (IID var _, _, _) -> Just var
    IDIV   (IID var _, _, _) -> Just var
    IMOD   (IID var _, _, _) -> Just var
    IPMINUS (IID var _, _)   -> Just var
    ICALLR (IID var _, _, _) -> Just var
    ICAST (IID var _,_,_)    -> Just var
    IARRAY_READ(IID var _,_,_) -> Just var
    IRECORD_READ(IID var _,_,_) -> Just var
    _ -> Nothing
