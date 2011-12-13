module SemanticAnalyzer (semanticAnalyzer) where

import AST
import SymbolTable
import SymbolTypes
import GetTackType
import Scope
import Data.Maybe
import Control.Monad
import PrettyPrint(indent)
import List(find, findIndex)
import SrcPos

semanticAnalyzer ::  Int -> ST  -> AST -> IO ()
semanticAnalyzer level st (Program {funDefList=fl}) = 
  do 
    let intrinsicFuncList = [ FUNCTION("append", TK_RECORD [("lhs", TK_STRING),("rhs",TK_STRING)], TK_STRING), FUNCTION("bool2int", TK_RECORD [("b", TK_BOOL)], TK_INT), FUNCTION("bool2string" , TK_RECORD [("b", TK_BOOL)], TK_STRING), FUNCTION("int2bool", TK_RECORD [("i", TK_INT)], TK_BOOL), FUNCTION("int2string", TK_RECORD [("i", TK_INT)], TK_STRING), FUNCTION("length", TK_RECORD [("s", TK_STRING)], TK_INT), FUNCTION("newArray", TK_RECORD [("eSize", TK_INT),("aSize",TK_INT)], TK_ARRAY $ TK_RECORD []), FUNCTION("newRecord", TK_RECORD [("rSize", TK_INT)], TK_RECORD []), FUNCTION("print", TK_RECORD [("s", TK_STRING)], TK_VOID), FUNCTION("range", TK_RECORD [("start", TK_INT),("end", TK_INT)], TK_ARRAY TK_INT), FUNCTION("size", TK_RECORD [("a", TK_ARRAY $ TK_RECORD [])], TK_INT), FUNCTION("string2bool", TK_RECORD [("s", TK_STRING)], TK_BOOL), FUNCTION("string2int", TK_RECORD [("s", TK_STRING)], TK_INT), FUNCTION("stringEqual", TK_RECORD [("lhs", TK_STRING),("rhs", TK_STRING)], TK_BOOL)]

    let n_st   = new_scope L_PROG st
    let new_st = foldl insertIntriFun n_st intrinsicFuncList
    new_st'   <- foldM (insertFunDef level) new_st fl 
    mapM_ (semanticAnalyzerFunDef level new_st') fl
    where
      insertIntriFun :: ST -> SYMBOL_DESC -> ST
      insertIntriFun st i = 
        insert st i       

semanticAnalyzerFunDef ::  Int -> ST -> FunDef -> IO ()
semanticAnalyzerFunDef level st (FunDef {funId=fId, funType=funT, bStmt=BlockStmt{stmtList=sL, stmtSrcPos=_}, funSrcPos=_}) =
  case fId of
    FunId {funIdName=fName,exprSrcPos=_} -> 
      do
        let level' = level + 1
        funRetType <- getTackType $ retType funT
        let new_st   = new_scope (L_FUN fName funRetType) st
        -- collect agruments 
        new_st'  <- foldM (insertFieldVar  level') new_st $ fieldTypeList  $ recordType funT 
        semanticAnalyzerType level' new_st' funT
        new_st''' <- foldM (insertVarDef  level') new_st' sL
        mapM_ (semanticAnalyzerStmt level'  new_st''') sL
    _ -> 
      do
        return ()

semanticAnalyzerFieldLit :: Int -> (ST, [(String,TACK_TYPE)])  -> FieldLit -> IO (ST, [(String,TACK_TYPE)])
semanticAnalyzerFieldLit level stAndType (FieldLit{fieldLitId=fId, fieldLitExpr=fE, fieldLitSrcPos=sP}) =
  do
    let  (st, fieldTL) = stAndType
    fT <- semanticAnalyzerExpr level st fE
    if isEmptyArray fT 
      then
        do
          printSemanticError sP ("Could not resolve type for field " ++ "'" ++ fieldIdName fId ++ "'")
          return (insert st (VARIABLE (fieldIdName fId, fT)), (fieldIdName fId,fT):fieldTL)
      else return ( insert st (VARIABLE (fieldIdName fId, fT)), (fieldIdName fId,fT):fieldTL)

semanticAnalyzerType :: Int -> ST -> Type -> IO ST
semanticAnalyzerType level st (RecordType {fieldTypeList=fieldL, typeSrcPos=_}) =
  do
    let level' = level + 1
    new_st <- foldM (insertFieldVar level') (new_scope L_RECORD st) fieldL
    --new_st <- foldM (semanticAnalyzerType level') (new_scope L_RECORD st) fieldL
    new_st <- foldM (semanticAnalyzerType level') new_st fieldL
    return new_st

semanticAnalyzerType level st (ArrayType{arrayType=aT, typeSrcPos=sP}) =
  semanticAnalyzerType level st aT

semanticAnalyzerType level st (FieldType {fieldId=FieldId{fieldIdName=fId, fieldIdSrcPos=_}, fieldType=fType, typeSrcPos=sP}) =
  do 
    fT     <- getTackType fType
    new_st <- return $ insert st (VARIABLE(fId, fT)) 
    semanticAnalyzerType level new_st fType

semanticAnalyzerType level st (PrimitiveType {name=n, typeSrcPos=sP}) = 
  return st

semanticAnalyzerType level st (FunType {recordType=rType, retType=retType, typeSrcPos=sP}) =
  do
    semanticAnalyzerType level st rType
    semanticAnalyzerType level st retType


semanticAnalyzerStmt ::  Int -> ST -> Stmt -> IO ()
semanticAnalyzerStmt level st (VarDef {varId=vId, varExpr=vE, stmtSrcPos=sp}) =
  return ()  

semanticAnalyzerStmt level st (AssignStmt {leftExpr=lE, rightExpr=rE, stmtSrcPos=sP}) = 
  do
    lT <- semanticAnalyzerExpr level st lE
    rT <- semanticAnalyzerExpr level st rE
    case lE of
      VarId{varIdName=i, exprSrcPos=sP} ->
        if isSubType rT lT 
          then return ()
          else printSemanticError sP ( "Cannot assign to " ++ show lT ++ " from " ++ show rT )
      SubscriptExpr {sExpr=sE, subscript=subE, exprSrcPos=sP} ->
        if isSubType rT lT 
          then return ()
          else printSemanticError sP ( "Cannot assign to " ++ show lT ++ " from " ++ show rT )
      FieldExpr {fieldExpr=fE, fieldExprId=fId, exprSrcPos=sP} ->
        if isSubType rT lT 
          then return ()
          else printSemanticError sP ( "Cannot assign to " ++ show lT ++ " from " ++ show rT )
      _ ->
        do
          printSemanticError sP "left hand side must be an l-value"

semanticAnalyzerStmt level st (BlockStmt {stmtList=sL, stmtSrcPos=sP}) =
  do
    let level' = level + 1
    let new_st = new_scope L_BLOCK st
    new_st' <- foldM (insertVarDef level') new_st sL
    mapM_ (semanticAnalyzerStmt level' new_st') sL

semanticAnalyzerStmt level st (CallStmt {cExpr=cE, stmtSrcPos=sP}) =
  do 
    callType <- semanticAnalyzerExpr level st cE
    return ()

semanticAnalyzerStmt level st (ForStmt {varId=vId, forExpr=fE, blockStmt=BlockStmt{stmtList=sL,stmtSrcPos=sSP}, stmtSrcPos=sP}) =
  case vId of
    VarId {varIdName=vIdName,exprSrcPos=sP} ->
      do
        let level' = level +1
        let new_st = new_scope (L_FORSTMT vIdName) st
        forE <- semanticAnalyzerExpr level' new_st fE
        case forE of
          arrayT@(TK_ARRAY t) ->
            do
             if isEmptyArray arrayT 
               then printSemanticError sP ("Could not resolve type for variable " ++ vIdName)
               else 
                 do
                   let new_st' = insert new_st (VARIABLE (vIdName, t))
                   new_st'' <- foldM (insertVarDef level') new_st' sL
                   mapM_ (semanticAnalyzerStmt level' new_st'') sL    
          _ -> printSemanticError (srcPos fE) "Subject of for-loop must be array."
    _ -> return ()

semanticAnalyzerStmt level st (IfStmt {bExpr=bE, thenStmts=tS, elseStmts=eS, stmtSrcPos=sP}) =
  do
    bT <- semanticAnalyzerExpr level st bE
    unless (isSame bT TK_BOOL) $ printSemanticError (srcPos bE) "Boolean expected."
    semanticAnalyzerStmt level st tS
    maybe (return ()) (semanticAnalyzerStmt level st) eS

semanticAnalyzerStmt level st (ReturnStmt {rExpr=rE, stmtSrcPos=sP}) =
  do 
    let retT = fReturn st
    case rE of
      Just rE' -> 
        do
          returnT <- semanticAnalyzerExpr level st rE'
          unless (isSame returnT retT) $ printSemanticError sP ("Expected return value of type " ++ show retT ++ ", found " ++ show returnT)
      Nothing ->
        unless (isSame retT TK_VOID) $ printSemanticError sP ("Expected return value of type " ++ show retT ++ ", found 'void'")
    

semanticAnalyzerStmt level st (WhileStmt {whileBoolExpr=bE, whileStmts=s, stmtSrcPos=sP}) =
  do 
    bT <- semanticAnalyzerExpr level st bE
    unless (isSame bT TK_BOOL)  $ printSemanticError (srcPos bE) "Boolean expected."
    semanticAnalyzerStmt level st s

semanticAnalyzerExpr :: Int -> ST -> Expr  -> IO TACK_TYPE
semanticAnalyzerExpr level st InfixExpr{op=o, inFixLeftExpr=lE, inFixRightExpr=rE, exprSrcPos=sP} =
  do 
    lEType <- semanticAnalyzerExpr level st lE
    rEType <- semanticAnalyzerExpr level st rE
    if  (o == "||") || (o == "&&") 
      then case (isSame lEType TK_BOOL,isSame rEType TK_BOOL) of
             (True, True ) -> return TK_BOOL 
             (True, False) ->
               do 
                 printSemanticError (srcPos rE)  "Boolean expected."
                 return TK_BOOL
             (False, True) ->
               do 
                 printSemanticError (srcPos lE)  "Boolean expected."
                 return TK_BOOL
             (False, False) ->
               do 
                 printSemanticError (srcPos lE)  "Boolean expected."
                 printSemanticError (srcPos rE)  "Boolean expected."
                 return TK_BOOL
       else if (o == "==") || ( o == "!=") 
              then if (isNULL lEType && isRecord rEType) || (isRecord lEType && isNULL rEType) || (isSame lEType rEType) 
                     then return TK_BOOL
                     else do
                            printSemanticError (srcPos lE) ("Cannot compare " ++ show lEType ++ " and " ++ show rEType ++ ".")
                            return TK_BOOL
              else if (o == "<=") || ( o == "<") || ( o == ">=") || ( o == ">") 
                     then case ( isSame lEType TK_INT, isSame rEType TK_INT ) of 
                            (True, True) -> return TK_BOOL
                            (True, False) ->
                              do
                                printSemanticError (srcPos rE) "Int expected."
                                return TK_BOOL 
                            (False, True) ->
                              do
                                printSemanticError (srcPos lE) "Int expected."
                                return TK_BOOL 
                            (False, False) ->
                              do
                                printSemanticError (srcPos lE) "Int expected."
                                printSemanticError (srcPos rE) "Int expected."
                                return TK_BOOL 
                     else if (o == "+" ) 
                            then if (isSame lEType TK_STRING) && (isSame rEType TK_STRING) 
                                   then return TK_STRING
                                   else if (isSame lEType TK_STRING ) && (isBasicType rEType) 
                                          then return TK_STRING
                                          else if (isBasicType lEType) && (isSame rEType TK_STRING)
                                                 then return TK_STRING
                                                 else case (isSame lEType TK_INT , isSame rEType TK_INT)  of
                                                        (True, True) -> return TK_INT
                                                        (True, False) -> 
                                                          do
                                                            printSemanticError (srcPos rE)  "Int expected."
                                                            return TK_INT
                                                        (False, True) ->
                                                          do
                                                            printSemanticError (srcPos lE)  "Int expected."
                                                            return TK_INT
                                                        (False, False) ->
                                                          do
                                                            printSemanticError (srcPos lE)  "Int expected."
                                                            printSemanticError (srcPos rE)  "Int expected."
                                                            return TK_INT
                            else case (isSame lEType TK_INT, isSame rEType TK_INT) of
                                   (True, True) -> return TK_INT
                                   (True, False) ->
                                     do
                                       printSemanticError (srcPos rE) "Int expected."
                                       return TK_INT          
                                   (False, True) ->
                                     do
                                       printSemanticError (srcPos lE) "Int expected."
                                       return TK_INT          
                                   (False, False) ->
                                     do
                                       printSemanticError (srcPos lE) "Int expected."
                                       printSemanticError (srcPos rE) "Int expected."
                                       return TK_INT          

semanticAnalyzerExpr level st (PrefixExpr{op=o,prefixExpr=pE,exprSrcPos=sP}) =
  do
    pT <- semanticAnalyzerExpr level st pE
    case o of
      "-" ->
        if (isSame pT TK_INT) 
          then return TK_INT
          else 
            do
              printSemanticError sP  "Int expected." 
              return TK_INT
      "!" ->
        if (isSame pT TK_BOOL) 
          then return TK_BOOL
          else 
            do
              printSemanticError sP  "Bool expected." 
              return TK_BOOL
      _ -> return TK_INT
        
semanticAnalyzerExpr level st (CallExpr {fNameExpr=fN,argExprs=aE,exprSrcPos=sP}) =
  do
--    putStrLn $ fN ++ (")=====\n)"
    funType <- semanticAnalyzerExpr level st fN
    case funType of
      (TK_FUN fName (TK_RECORD []) TK_NULL)-> return TK_NULL
      (TK_FUN fName (TK_RECORD formalL) retT)->
        do 
          let formalTL = map snd formalL 
          argL <- mapM (semanticAnalyzerExpr level st) aE
          if (length formalTL) == (length argL)
            then if isSubTypeList formalTL argL 
                   then return retT
                   else
                     do 
                       printSemanticError sP  ("Formal " ++ (listToString $ map fst formalL) 
                         ++ " of " ++ fName ++ " expect " ++ listToString formalTL ++ " found " ++ listToString argL ++ " instead" )
                       return retT
            else 
              do
                printSemanticError sP  "The number of arguments of "
                return retT 
      _ -> 
        do 
          printSemanticError sP  ("Funtion " ++ show funType )  
          return TK_NULL

semanticAnalyzerExpr level st (CastExpr {castExpr=cE,castType=cT,exprSrcPos=sP}) =
  do
    fromT <- semanticAnalyzerExpr level st cE
    toT   <- getTackType cT
    unless (isCastable toT fromT) $ printSemanticError (srcPos cE) ("Cannot cast from type " ++ show fromT ++ " to type " ++ show toT )
    semanticAnalyzerType level st cT
    return toT

semanticAnalyzerExpr level st (FieldExpr{fieldExpr=fE, fieldExprId=FieldId{fieldIdName=fId,fieldIdSrcPos=fSP},exprSrcPos=_}) =
  do 
    fType <- semanticAnalyzerExpr level st fE
    case fType of 
      TK_RECORD fieldL ->
        case (find (\x-> (fst x) ==  fId) fieldL) of
          Just t  -> 
            return $ snd t
          Nothing ->
            do 
              printSemanticError fSP ("Unknown field " ++ "'" ++ fId ++ "'")
              return TK_NULL
      _ -> do
             printSemanticError fSP $ "Field expression expected." ++ show fType
             return $ TK_RECORD []

semanticAnalyzerExpr level st (SubscriptExpr{sExpr=sE, subscript=subE,exprSrcPos=sP}) =
  do
    baseT <- semanticAnalyzerExpr level st sE
    subT  <- semanticAnalyzerExpr level st subE
    case (baseT,subT)  of
      (TK_ARRAY t, TK_INT) -> return t 
      (_, TK_INT) ->
        do
          printSemanticError (srcPos sE) "Base of subscript must be array."
          return $ TK_ARRAY TK_NULL
      (TK_ARRAY t, _) ->
        do
          printSemanticError (srcPos subE) "subscript must be int."
          return $ TK_ARRAY TK_NULL  
      _ -> 
        do
          printSemanticError (srcPos sE) "Base of subscript must be array."
          printSemanticError (srcPos subE) "subscript must be int."
          return $ TK_ARRAY TK_NULL

semanticAnalyzerExpr level st (ParenExpr{pExpr=pE,exprSrcPos=sP}) =
  semanticAnalyzerExpr level st pE

semanticAnalyzerExpr level st (ArrayLit {arrayExprs=aEL,exprSrcPos=sP}) =
  do
    eTL <- mapM (semanticAnalyzerExpr level st) aEL
    if eTL == [] 
      then return $ TK_ARRAY TK_NULL
      else
        if (length eTL) == (countElem (head eTL) eTL)
          then 
            case find isEmptyArray eTL of
              Just _ -> 
                do 
                  printSemanticError sP "Could not resolve array element type."
                  return $ TK_ARRAY $ head eTL
              Nothing -> return $ TK_ARRAY $ head eTL
          else 
            case find (\s -> (head eTL) /= s) eTL  of
              Just n -> 
                do
                  printSemanticError (srcPos aEL) ("Expected element of type " ++  show (head eTL) ++ ", found " ++ (show n) )
                  return $ TK_ARRAY $ head eTL
              Nothing -> return $ TK_ARRAY $ head eTL

semanticAnalyzerExpr level st (RecordLit {recordFieldLits=rFL,exprSrcPos=sP}) =
  do
    let level' = level + 1
    let new_st = new_scope L_RECORDLIT st 
    -- (st, fieldL) <- foldl (semanticAnalyzerFieldLitVar level') (return (new_st,[])) rFL
    (st, fieldL) <- foldM (semanticAnalyzerFieldLit level') (new_st,[]) rFL
    return $ TK_RECORD (reverse fieldL)
    
semanticAnalyzerExpr level st (BoolLit{boolValue=bV,exprSrcPos=sP}) =
  return TK_BOOL

semanticAnalyzerExpr level st (IntLit{intValue=iV,exprSrcPos=sP}) =
  return TK_INT

semanticAnalyzerExpr level st (NullLit{exprSrcPos=sP}) =
  return TK_NULL

semanticAnalyzerExpr level st (StringLit{stringValue=sV,exprSrcPos=sP})  =
  return TK_STRING

semanticAnalyzerExpr level st (FunId{funIdName=fId,exprSrcPos=sP}) =
  case look_up st fId of
    I_FUNCTION(_, _, argType, retType) -> return $ TK_FUN fId argType retType
    I_VARIABLE(_,_,varType) -> 
      do 
        printSemanticError sP "Function name expected."
        return $ TK_FUN fId (TK_RECORD []) TK_NULL
    _ -> 
      do 
        printSemanticError sP ("Unknown function " ++ "'" ++ fId ++ "'")
        return $ TK_NULL

semanticAnalyzerExpr level st (VarId{varIdName=vId,exprSrcPos=sP}) =
  case look_up st vId of
    I_VARIABLE(_,_, tackType) -> return tackType
    I_FUNCTION(_,_,argT,retT) -> 
      do
        printSemanticError sP "Variable name expected."                 
        return $ retT
    _ -> do
           printSemanticError sP ("Unknown variable " ++ "'" ++ vId ++ "'")
           return TK_NULL
          


printSemanticError ::  SrcPos -> String -> IO ()
printSemanticError p s = 
  error $ show p ++ ": " ++ s

insertFunDef :: Int -> ST -> FunDef -> IO ST
insertFunDef level st (FunDef {funId=FunId {funIdName=fname, exprSrcPos=_}, funType=FunType{recordType=argT,retType=retT,typeSrcPos=_}, bStmt=_, funSrcPos=_}) =
  do
    argT' <- getTackType argT
    retT' <- getTackType retT
    return $ insert st (FUNCTION (fname, argT', retT'))

insertVarDef :: Int -> ST -> Stmt -> IO ST
insertVarDef level st (VarDef {varId=vId, varExpr=ve, stmtSrcPos=_}) =
  case vId of
    VarId {varIdName=varId, exprSrcPos=sP} ->
      do
        exprType <- semanticAnalyzerExpr level st ve
        case exprType of
          arrayT@(TK_ARRAY t) ->
            if isEmptyArray arrayT 
              then
                do
                   printSemanticError sP ("Could not resolve type for variable " ++ "'" ++ varId ++ "'")
                   return $ insert st (VARIABLE (varId, exprType))
              else return $ insert st (VARIABLE (varId, exprType))
          (TK_RECORD fList) ->
            let fieldTypeL= ( filter (\x -> isEmptyArray (snd x) ) fList )  in
              if fieldTypeL == [] 
                then return $ insert st (VARIABLE (varId, exprType))
                else do
                       printSemanticError sP ("Could not resolve type for variable " ++ "'" ++ varId ++ "'")
                       return $ insert st (VARIABLE (varId, exprType))
          _ -> return $ insert st (VARIABLE (varId, exprType))
    _  -> return st

insertVarDef level st (WhileStmt {whileBoolExpr=bE, whileStmts=s, stmtSrcPos=sP}) =
  foldM (insertVarDef level) st (stmtList s)

insertVarDef leve st _ = return st


insertFieldVar :: Int -> ST -> Type -> IO ST
insertFieldVar level st (FieldType {fieldId=fId, fieldType=fType, typeSrcPos=sP}) =
  do
    fT     <- getTackType fType
    return $ insert st (VARIABLE(fieldIdName fId, fT))

insertFieldVar level st _ = return st

