module PrintSymbolTable (printSymbolTable) where

import AST
import SymbolTable
import SymbolTypes
import GetTackType
import Scope
import Control.Monad
import Data.Maybe
import PrettyPrint(indent)
import List(find)


printSymbolTable ::  Int -> ST  -> AST -> IO ()
printSymbolTable level st (Program {funDefList=fl}) = 
  do 
    let intrinsicFuncList = [ FUNCTION("append", TK_RECORD [("lhs", TK_STRING),("rhs",TK_STRING)], TK_STRING), FUNCTION("bool2int", TK_RECORD [("b", TK_BOOL)], TK_INT), FUNCTION("bool2string" , TK_RECORD [("b", TK_BOOL)], TK_STRING), FUNCTION("int2bool", TK_RECORD [("i", TK_INT)], TK_BOOL), FUNCTION("int2string", TK_RECORD [("i", TK_INT)], TK_STRING), FUNCTION("length", TK_RECORD [("s", TK_STRING)], TK_INT), FUNCTION("newArray", TK_RECORD [("eSize", TK_INT),("aSize",TK_INT)], TK_ARRAY $ TK_RECORD []), FUNCTION("newRecord", TK_RECORD [("rSize", TK_INT)], TK_RECORD []), FUNCTION("print", TK_RECORD [("s", TK_STRING)], TK_VOID), FUNCTION("range", TK_RECORD [("start", TK_INT),("end", TK_INT)], TK_ARRAY TK_INT), FUNCTION("size", TK_RECORD [("a", TK_ARRAY $ TK_RECORD [])], TK_INT), FUNCTION("string2bool", TK_RECORD [("s", TK_STRING)], TK_BOOL), FUNCTION("string2int", TK_RECORD [("s", TK_STRING)], TK_INT), FUNCTION("stringEqual", TK_RECORD [("lhs", TK_STRING),("rhs", TK_STRING)], TK_BOOL)]

    let n_st   = new_scope L_PROG st
    let new_st = foldl insertIntriFun n_st intrinsicFuncList
    new_st'   <- foldl (insertFunDef level) (return new_st) fl 
    putStrLn $ indent level . show $ head new_st' -- print symbol table 
    mapM_ (printSymbolTableFunDef level new_st') fl
    where
      insertIntriFun :: ST -> SYMBOL_DESC -> ST
      insertIntriFun st i = 
        snd $ insert level st i       

printSymbolTableFunDef ::  Int -> ST -> FunDef -> IO ()
printSymbolTableFunDef level st (FunDef {funId=fId, funType=funT, bStmt=BlockStmt{stmtList=sL, stmtSrcPos=_}, funSrcPos=_}) =
  case fId of
    FunId {funIdName=fName,exprSrcPos=_} -> 
      do
        let level' = level + 1
        funRetType <- getTackType $ retType funT
        let new_st   = new_scope (L_FUN fName funRetType) st
        -- collect agruments 
        let new_st'  = foldl (insertFieldVar  level') (return new_st) $ fieldTypeList  $ recordType funT 
        -- collect vardef 
        t_st  <- foldl (insertVar  level')  new_st' sL -- for print a symbol table
        putStrLn $ indent level' . show $ head t_st
        ---
        printSymbolTableType level' new_st' funT
        new_st''' <- foldl (insertVar  level') new_st' sL
        mapM_ (printSymbolTableStmt level'  new_st''') sL
    _ -> 
      do
        return ()

printSymbolTableFieldLit :: Int -> IO (ST, [(String,TACK_TYPE)])  -> FieldLit -> IO (ST, [(String,TACK_TYPE)])
printSymbolTableFieldLit level stAndType (FieldLit{fieldLitId=fId, fieldLitExpr=fE, fieldLitSrcPos=sP}) =
  do
    (st, fieldTL)  <- stAndType
    fT <- printSymbolTableExpr level st fE
    return $ (snd $ insert level st (VARIABLE (fieldIdName fId, fT)), (fieldIdName fId,fT):fieldTL)

printSymbolTableFieldLitVar :: Int -> IO (ST, [(String,TACK_TYPE)])  -> FieldLit -> IO (ST, [(String,TACK_TYPE)])
printSymbolTableFieldLitVar level stAndType (FieldLit{fieldLitId=fId, fieldLitExpr=fE, fieldLitSrcPos=sP}) =
  do
    (st, fieldTL)  <- stAndType
    return $ (snd $ insert level st (VARIABLE (fieldIdName fId, TK_NULL)), (fieldIdName fId, TK_NULL):fieldTL)

printSymbolTableType :: Int -> IO ST -> Type -> IO ST
printSymbolTableType level st (RecordType {fieldTypeList=fieldL, typeSrcPos=_}) =
  do
    let level' = level + 1
    new_st <- foldl (insertFieldVar level') (liftM (new_scope L_RECORD) st) fieldL
    putStrLn $ indent level' . show $ head new_st
    new_st <- foldl (printSymbolTableType level') (liftM (new_scope L_RECORD) st) fieldL
    return new_st

printSymbolTableType level st (ArrayType{arrayType=aT, typeSrcPos=sP}) =
  printSymbolTableType level st aT

printSymbolTableType level st (FieldType {fieldId=FieldId{fieldIdName=fId, fieldIdSrcPos=_}, fieldType=fType, typeSrcPos=sP}) =
  do 
    st'    <- st
    fT     <- getTackType fType
    new_st <- return $ snd $ insert level st' (VARIABLE(fId, fT)) 
    printSymbolTableType level (return new_st) fType

printSymbolTableType level st (PrimitiveType {name=n, typeSrcPos=sP}) = 
  st

printSymbolTableType level st (FunType {recordType=rType, retType=retType, typeSrcPos=sP}) =
  do
    printSymbolTableType level st rType
    printSymbolTableType level st retType


printSymbolTableStmt ::  Int -> ST -> Stmt -> IO ()
printSymbolTableStmt level st (VarDef {varId=vId, varExpr=vE, stmtSrcPos=sP}) =
  case vId of
    VarId {varIdName=varId, exprSrcPos=_} ->
      do
        exprType <- printSymbolTableExpr level st vE
        return ()
    _ -> return ()
  

printSymbolTableStmt level st (AssignStmt {leftExpr=lE, rightExpr=rE, stmtSrcPos=sP}) = 
  do
    printSymbolTableExpr level st lE
    printSymbolTableExpr level st rE
    return ()

printSymbolTableStmt level st (BlockStmt {stmtList=sL, stmtSrcPos=sP}) =
  do
    let level' = level + 1
    let new_st = new_scope L_BLOCK st
    new_st <- foldl (insertVar level') (return new_st) sL
    putStrLn $ indent level' . show $ head $ new_st
    mapM_ (printSymbolTableStmt level' new_st) sL

printSymbolTableStmt level st (CallStmt {cExpr=cE, stmtSrcPos=sP}) =
  do 
    callType <- printSymbolTableExpr level st cE
    return ()

printSymbolTableStmt level st (ForStmt {varId=vId, forExpr=fE, blockStmt=BlockStmt{stmtList=sL,stmtSrcPos=sSP}, stmtSrcPos=sP}) =
  case vId of
    VarId {varIdName=vIdName,exprSrcPos=sP} ->
      do
        let level' = level +1
        let new_st = new_scope (L_FORSTMT vIdName) st
        forE <- printSymbolTableExpr level' new_st fE
        let new_st' = snd $ insert level new_st (VARIABLE (vIdName, TK_NULL))
        new_st'' <- foldl (insertVar level') (return new_st') sL
        putStrLn $ indent level' . show $ head new_st''
        mapM_ (printSymbolTableStmt level' new_st') sL    
    _ -> return ()

printSymbolTableStmt level st (IfStmt {bExpr=bE, thenStmts=tS, elseStmts=eS, stmtSrcPos=sP}) =
  do
    printSymbolTableExpr level st bE
    printSymbolTableStmt level st tS
    maybe (return ()) (printSymbolTableStmt level st) eS

printSymbolTableStmt level st (ReturnStmt {rExpr=rE, stmtSrcPos=sP}) =
    case  rE of
      Just rE' -> 
        do 
          returnT <- printSymbolTableExpr level st rE'
          return ()
      Nothing -> return ()

printSymbolTableStmt level st (WhileStmt {whileBoolExpr=bE, whileStmts=s, stmtSrcPos=sP}) =
  do 
    wBE <- printSymbolTableExpr level st bE
    printSymbolTableStmt level st s

printSymbolTableExpr :: Int -> ST -> Expr  -> IO TACK_TYPE
printSymbolTableExpr level st InfixExpr{op=o, inFixLeftExpr=lE, inFixRightExpr=rE, exprSrcPos=sP} =
  do 
    lEType <- printSymbolTableExpr level st lE
    rEType <- printSymbolTableExpr level st rE
    return TK_INT
        
printSymbolTableExpr level st (PrefixExpr{op=o,prefixExpr=pE,exprSrcPos=sP}) =
  do
    pT <- printSymbolTableExpr level st pE
    case o of
      "-" ->
        if (isSame pT TK_INT) 
          then return TK_INT
          else return TK_INT
      "!" ->
        if (isSame pT TK_BOOL) 
          then return TK_BOOL
          else return TK_BOOL
      _ -> return TK_INT
        
printSymbolTableExpr level st (CallExpr {fNameExpr=fN,argExprs=aE,exprSrcPos=sP}) =
  do
    funType <- printSymbolTableExpr level st fN
    mapM (printSymbolTableExpr level st) aE
    return funType


printSymbolTableExpr level st (CastExpr {castExpr=cE,castType=cT,exprSrcPos=sP}) =
  do
    printSymbolTableExpr level st cE
    printSymbolTableType level (return st) cT
    getTackType cT

printSymbolTableExpr level st (FieldExpr{fieldExpr=fE, fieldExprId=FieldId{fieldIdName=fId,fieldIdSrcPos=_},exprSrcPos=sP}) =
  do 
    fType <- printSymbolTableExpr level st fE
    case fType of 
      TK_RECORD fieldL ->
        case (find (\x-> (fst x) ==  fId) fieldL) of
          Just t  -> return $ snd t
          Nothing ->
            return TK_NULL
      _ -> return $ TK_RECORD []

printSymbolTableExpr level st (SubscriptExpr{sExpr=sE, subscript=subE,exprSrcPos=sP}) =
  do
    printSymbolTableExpr level st sE
    printSymbolTableExpr level st subE

printSymbolTableExpr level st (ParenExpr{pExpr=pE,exprSrcPos=sP}) =
  printSymbolTableExpr level st pE

printSymbolTableExpr level st (ArrayLit {arrayExprs=aEL,exprSrcPos=sP}) =
  do
    eTL <- mapM (printSymbolTableExpr level st) aEL
    if eTL == [] 
      then return TK_NULL
      else return $ TK_ARRAY $ head eTL

printSymbolTableExpr level st (RecordLit {recordFieldLits=rFL,exprSrcPos=sP}) =
  do
    let level' = level + 1
    let new_st = new_scope L_RECORDLIT st 
    (st, fieldL) <- foldl (printSymbolTableFieldLitVar level') (return (new_st,[])) rFL
    putStrLn $ indent level' . show $ head st
    (st, fieldL) <- foldl (printSymbolTableFieldLit level') (return (new_st,[])) rFL
    return $ TK_RECORD fieldL
    
printSymbolTableExpr level st (BoolLit{boolValue=bV,exprSrcPos=sP}) =
  return TK_BOOL

printSymbolTableExpr level st (IntLit{intValue=iV,exprSrcPos=sP}) =
  return TK_INT

printSymbolTableExpr level st (NullLit{exprSrcPos=sP}) =
  return TK_NULL

printSymbolTableExpr level st (StringLit{stringValue=sV,exprSrcPos=sP})  =
  return TK_STRING

printSymbolTableExpr level st (FunId{funIdName=fId,exprSrcPos=sP}) =
  case look_up st fId of
    I_FUNCTION(_, _, _, retType) -> return $ retType
    _ -> return TK_NULL

printSymbolTableExpr level st (VarId{varIdName=vId,exprSrcPos=sP}) =
  case look_up st vId of
    I_VARIABLE(_,_, tackType) -> return tackType
    _ -> return TK_NULL

insertFunDef :: Int -> IO ST -> FunDef -> IO ST
insertFunDef level st (FunDef {funId=FunId {funIdName=fname, exprSrcPos=_}, funType=FunType{recordType=argT,retType=retT,typeSrcPos=_}, bStmt=_, funSrcPos=_}) =
  do
    argT' <- getTackType argT
    retT' <- getTackType retT
    st'   <- st
    return $ snd $ insert level st' (FUNCTION (fname, argT', retT'))

insertVarDef :: Int -> IO ST -> Stmt -> IO ST
insertVarDef level st (VarDef {varId=vId, varExpr=ve, stmtSrcPos=_}) =
  case vId of
    VarId {varIdName=varId, exprSrcPos=_} ->
      do
        st' <- st
        return $ snd $ insert level st' (VARIABLE (varId, TK_NULL))
    _ -> st


insertVarDef level st (IfStmt {bExpr=bE, thenStmts=tS, elseStmts=eS, stmtSrcPos=sP}) = 
  do
    new_st  <- insertVarDef level st tS
    case eS of
      Just eS' -> 
        do
          new_st' <- insertVarDef level (return new_st) eS'
          return new_st'
      Nothing -> return new_st

insertVarDef level st (WhileStmt {whileBoolExpr=bE, whileStmts=s, stmtSrcPos=sP}) =
  insertVarDef level st s

insertVarDef leve st _ = st

insertVar :: Int -> IO ST -> Stmt -> IO ST
insertVar level st (VarDef {varId=vId, varExpr=ve, stmtSrcPos=_}) =
  case vId of
    VarId {varIdName=varId, exprSrcPos=_} ->
      do
	st' <- st
	return $ snd $ insert level st' (VARIABLE (varId, TK_NULL))
    _ -> st

insertVar level st (IfStmt {bExpr=bE, thenStmts=tS, elseStmts=eS, stmtSrcPos=sP}) =
  do
    new_st  <- insertVar level st tS
    case eS of
      Just eS' ->
        do
          new_st' <- insertVar level (return new_st) eS'
          return new_st'
      Nothing -> return new_st

insertVar level st (WhileStmt {whileBoolExpr=bE, whileStmts=s, stmtSrcPos=sP}) =
  insertVar level st s

insertVar level st _ = st

insertFieldVar :: Int -> IO ST -> Type -> IO ST
insertFieldVar level st (FieldType {fieldId=fId, fieldType=fType, typeSrcPos=sP}) =
  do
    st'    <- st
    fT     <- getTackType fType
    return $ snd $ insert level st' (VARIABLE(fieldIdName fId, fT))

insertFieldVar level st _ = st

