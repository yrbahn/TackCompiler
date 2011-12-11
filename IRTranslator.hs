module IRTranslator where
import Data.Monoid
import AST
import IR
import SymbolTable
import SymbolTypes
import GetTackType
import Scope
import Data.Maybe
import Control.Monad
import List(find, findIndex)
import GlobalVar

transProg ::  Int -> ST  -> AST -> IO(IProg)
transProg level st (Program {funDefList=fl}) = 
  do 
    let intrinsicFuncList = [FUNCTION("append", TK_RECORD [("lhs", TK_STRING),("rhs",TK_STRING)], TK_STRING), FUNCTION("bool2int", TK_RECORD [("b", TK_BOOL)], TK_INT), FUNCTION("bool2string" , TK_RECORD [("b", TK_BOOL)], TK_STRING), FUNCTION("int2bool", TK_RECORD [("i", TK_INT)], TK_BOOL), FUNCTION("int2string", TK_RECORD [("i", TK_INT)], TK_STRING), FUNCTION("length", TK_RECORD [("s", TK_STRING)], TK_INT), FUNCTION("newArray", TK_RECORD [("eSize", TK_INT),("aSize",TK_INT)], TK_ARRAY $ TK_RECORD []), FUNCTION("newRecord", TK_RECORD [("rSize", TK_INT)], TK_RECORD []), FUNCTION("print", TK_RECORD [("s", TK_STRING)], TK_VOID), FUNCTION("range", TK_RECORD [("start", TK_INT),("end", TK_INT)], TK_ARRAY TK_INT), FUNCTION("size", TK_RECORD [("a", TK_ARRAY $ TK_RECORD [])], TK_INT), FUNCTION("string2bool", TK_RECORD [("s", TK_STRING)], TK_BOOL), FUNCTION("string2int", TK_RECORD [("s", TK_STRING)], TK_INT), FUNCTION("stringEqual", TK_RECORD [("lhs", TK_STRING),("rhs", TK_STRING)], TK_BOOL) ] 
    let new_st   = new_scope L_PROG st
    let new_st'  = foldl insertIntriFun new_st intrinsicFuncList
    new_st''  <- foldM (insertFun level) new_st' fl 
    funList <-  mapM (transFun level new_st'') fl
    return $ IPROG funList
    where
      insertIntriFun :: ST -> SYMBOL_DESC -> ST
      insertIntriFun st i = 
        insert st i       

transFun ::  Int -> ST -> FunDef -> IO (IFun)
transFun level st (FunDef {funId=fId, funType=funT, bStmt=blockStmt, funSrcPos=_}) =
  do
    let level' = level + 1
    let fName = funIdName fId
    funRetType <- getTackType $ retType funT
    funType    <- getTackType funT
    let new_st   = new_scope (L_FUN fName funRetType) st
     -- collect agruments 
    new_st'  <- foldM (insertArgument  level')  new_st $ fieldTypeList  $ recordType funT 
    -- semanticAnalyzerType level' new_st' funT
    let sL = stmtList blockStmt
    new_st''' <- foldM (insertSymbol  level') new_st' sL
    (sList, label) <- foldM (transStmt' level new_st''') ([],Nothing) sL
    if isSame funRetType TK_VOID || isJust label
      then  return $ IFUN(new_st''', fName, funType, sList ++ [makeStmt label $ IRETURN(Nothing)])
      else 
        return $ IFUN(new_st''', fName, funType, sList) 

transFieldLit :: Int ->  ST -> FieldLit -> IO (([IStmt], Maybe [ILabel]), (IId, IAddr))
transFieldLit level st (FieldLit{fieldLitId=fId, fieldLitExpr=fE, fieldLitSrcPos=sP}) =
  if isAddr fE
    then return (([], Nothing), (fieldIdName fId, getAddr st fE))
    else do
           newAddr <- genNewAddr
           codeAndLabel <- transExpr level st newAddr fE
           return ( codeAndLabel, (fieldIdName fId, newAddr))

transFieldLits :: Int -> ST -> [FieldLit] -> IO(([IStmt], Maybe [ILabel]), [(IId, IAddr)])
transFieldLits level st fieldList =
  foldM transFieldLit' (([],Nothing),[]) fieldList
  where transFieldLit' acc field =
          do 
            let preLabel = snd $ fst acc
            let preStmt  = fst $ fst acc
            let preTuple = snd acc
            ((codeL,label) , fieldTuple) <- transFieldLit level st field
            return ( (preStmt ++ addLabelToStmtList preLabel codeL, label), preTuple ++ [fieldTuple] )


---
--- Translate a statement into a IR statement list
--- 
transStmt ::  Int -> ST -> Stmt -> IO ([IStmt], Maybe [ILabel])
transStmt level st (VarDef{varId=vId, varExpr=vE, stmtSrcPos=sp}) =
  let varAddr = getAddr st vId in
  case vE of
    ArrayLit{arrayExprs=aEL, exprSrcPos=_} ->
      do
        ((codeL,labels), argL) <- transExprs level st aEL
        let arity = length argL
        let (TK_ARRAY tackType) = getTackTypeFromSymbolDesc $ look_up st $ varIdName vId
        return ( [makeStmt' $ IPARAM(0, 2, ISIZEOF tackType)] 
          ++ [makeStmt' $ IPARAM(1, 2, IINT arity)] ++ [makeStmt' $ ICALLR(varAddr, "newArray", 2)] 
          ++ codeL ++ (addLabelToStmtList labels $ fst $ foldl (genIArrayWriteCode varAddr) ([],0) argL),  Nothing )
               
    RecordLit{recordFieldLits=rFL,exprSrcPos=sP} ->         
      do
        ((codeL, labels), argL) <-  transFieldLits level st rFL
        let t = getTackTypeFromSymbolDesc $ look_up st  $ varIdName vId
        return ( [makeStmt' $ IPARAM(0, 1, ISIZEOF t)] ++ [makeStmt' $ ICALLR(varAddr, "newRecord", 1)] 
          ++ codeL ++ (addLabelToStmtList labels $ foldl (genIRecordWriteCode varAddr) [] argL), Nothing)
    _ ->
      if isAddr vE
        then return ( [makeStmt' $ ICOPY(varAddr, getAddr st vE)], Nothing)
        else transExpr level st varAddr vE

transStmt level st (AssignStmt {leftExpr=lE, rightExpr=rE, stmtSrcPos=_}) = 
  case lE of
    VarId{varIdName=i, exprSrcPos=_} ->
      if isAddr rE 
        then return ([makeStmt' $ ICOPY(getAddr st lE, getAddr st rE)], Nothing)
        else transExpr level st (getAddr st lE) rE

    SubscriptExpr {sExpr=sE, subscript=subE, exprSrcPos=_} ->
      if isAddr sE
        then if isAddr subE
               then if isAddr rE
                      then return ([makeStmt' $ IARRAY_WRITE(getAddr st sE, getAddr st subE, getAddr st rE)], Nothing) 
                      else do
                             newAddr <- genNewAddr
                             (code,label) <- transExpr level st newAddr rE
                             return (code ++ [makeStmt label $ IARRAY_WRITE(getAddr st sE, getAddr st subE, newAddr)], Nothing)
               else if isAddr rE
                      then do
                             newAddr <- genNewAddr
                             (code, label) <- transExpr level st newAddr subE
                             return (code ++ [makeStmt label $ IARRAY_WRITE(getAddr st sE, newAddr, getAddr st rE)], Nothing)
                      else do
                             newAddr1 <- genNewAddr
                             newAddr2 <- genNewAddr
                             (code1, label1) <- transExpr level st newAddr1 rE
                             (code2, label2) <- transExpr level st newAddr2 subE
                             return (code1 ++ addLabelToStmtList label1 code2 
                               ++ [makeStmt label2 $ IARRAY_WRITE(getAddr st sE, newAddr2, newAddr1)], Nothing)
        else if isAddr subE
               then if isAddr rE
                      then do
                             newAddr <- genNewAddr
                             (code,label) <- transExpr level st newAddr sE
                             return (code ++ [makeStmt label $ IARRAY_WRITE(newAddr, getAddr st subE, getAddr st rE)], Nothing) 
                      else do
                             newAddr1 <- genNewAddr
                             newAddr2 <- genNewAddr       
                             (code1,label1) <- transExpr level st newAddr1 rE
                             (code2,label2) <- transExpr level st newAddr2 sE
                             return (code1 ++ addLabelToStmtList label1 code2 
                               ++ [makeStmt label2 $ IARRAY_WRITE(newAddr2, getAddr st subE, newAddr1)], Nothing)
               else if isAddr rE
                      then do
                             newAddr1 <- genNewAddr
                             newAddr2 <- genNewAddr
                             (code1, label1) <- transExpr level st newAddr1 sE
                             (code2, label2) <- transExpr level st newAddr2 subE
                             return (code1 ++ addLabelToStmtList label1 code2 
                               ++ [makeStmt label2 $ IARRAY_WRITE(newAddr1, newAddr2, getAddr st rE)], Nothing)
                      else do
                             newAddr1 <- genNewAddr
                             newAddr2 <- genNewAddr
                             newAddr3 <- genNewAddr
                             (code1, label1) <- transExpr level st newAddr1 rE
                             (code2, label2) <- transExpr level st newAddr2 sE
                             (code3, label3) <- transExpr level st newAddr3 subE
                             return (code1 ++ addLabelToStmtList label1 code2 ++ addLabelToStmtList label2 code3 
                               ++ [makeStmt label3 $ IARRAY_WRITE(newAddr2, newAddr3, newAddr1)], Nothing)


 
    FieldExpr {fieldExpr=fE, fieldExprId=fId, exprSrcPos=_} ->
      if isAddr fE 
        then if isAddr rE 
               then return ([makeStmt' $ IRECORD_WRITE(getAddr st fE, fieldIdName fId, getAddr st rE)], Nothing)
               else do
                      newAddr <- genNewAddr
                      (code, label) <- transExpr level st newAddr rE
                      return ( code ++ [makeStmt label $ IRECORD_WRITE(getAddr st fE, fieldIdName fId, newAddr)],Nothing)
        else if isAddr rE
               then do
                      newAddr <- genNewAddr
                      (code, label) <- transExpr level st newAddr fE
                      return ( code ++ [makeStmt label $ IRECORD_WRITE(newAddr, fieldIdName fId, getAddr st rE)],Nothing)
               else do
                      newAddr1 <- genNewAddr
                      newAddr2 <- genNewAddr
                      (code1, label1) <- transExpr level st newAddr1 rE
                      (code2, label2) <- transExpr level st newAddr2 fE
                      return (code1 ++ addLabelToStmtList label1 code2 ++ [makeStmt label2 $ IRECORD_WRITE(newAddr2, fieldIdName fId, newAddr1)], Nothing)
    _ ->
      do
        newAddr1 <- genNewAddr
        newAddr2 <- genNewAddr
        (code1, label1) <- transExpr level st newAddr1 lE
        (code2, label2) <- transExpr level st newAddr2 rE
        return (code1 ++ addLabelToStmtList label1 code2 ++ [makeStmt label2 $ ICOPY(newAddr1, newAddr2)], Nothing)

transStmt level st (BlockStmt {stmtList=sL, stmtSrcPos=_}) =
  do
    let level' = level + 1
    let new_st = new_scope L_BLOCK st
    new_st' <- foldM (insertSymbol level') new_st sL
    foldM (transStmt' level new_st') ([],Nothing) sL

transStmt level st (CallStmt {cExpr=CallExpr {fNameExpr=fN,argExprs=aEL,exprSrcPos=_}, stmtSrcPos=_}) =
  do
    ((codeL,label), argL) <- transExprs level st aEL
    let arity = length argL
    case fN of
      FunId{funIdName=fId,exprSrcPos=_} -> 
        do
          return (codeL ++ addLabelToStmtList label (foldl (genIParamCode arity) [] argL) 
                                             ++ [makeStmt' $ ICALL(fId, arity)], Nothing)
      _ -> error "error!"
       
transStmt level st (ForStmt {varId=VarId{varIdName=vId,exprSrcPos=_}, forExpr=fE, blockStmt=BlockStmt{stmtList=sL,stmtSrcPos=_}, stmtSrcPos=_}) =
  do
    let level' = level + 1
    let newSt  = new_scope (L_FORSTMT vId) st
    exprType <- getTackTypeFromExpr level st fE
    case exprType of
      (TK_ARRAY elemType) ->
        do
          if isAddr fE 
            then do
                   (forVarId, newSt') <- insertForStmtId level' newSt vId elemType
                   newSt'' <- foldM (insertSymbol level') newSt' sL
                   (code2, label2) <- foldM (transStmt' level newSt'') ([],Nothing) sL
                   retAddr <- genNewAddr
                   iAddr   <- genNewAddr
                   tLabel  <- genNewLabel
                   fLabel  <- genNewLabel
                   lLabel  <- genNewLabel
                   let forExprAddr = getAddr newSt fE
                   return ([makeStmt' $ IPARAM(0,1, forExprAddr), makeStmt' $ ICALLR(retAddr, "size", 1)] 
                     ++ [makeStmt' $ ICOPY(iAddr, IINT 0), makeStmt (Just [lLabel]) $ ILT_JUMP(iAddr, retAddr, tLabel)] 
                     ++ [makeStmt' $ IUNCOND_JUMP(fLabel)]
                     ++ [makeStmt  (Just [tLabel]) $ IARRAY_READ(forVarId,forExprAddr,iAddr)] ++ code2
                     ++ [makeStmt label2 $IPLUS(iAddr,iAddr,IINT 1)]
                     ++ [makeStmt' $ IUNCOND_JUMP(lLabel)], Just [fLabel])
            else do
                   forExprAddr <- genNewAddr
                   (code1, label1) <- transExpr level' newSt forExprAddr fE
                   (forVarId, newSt') <- insertForStmtId level' newSt vId elemType
                   newSt'' <- foldM (insertSymbol level') newSt' sL
                   (code2, label2) <- foldM (transStmt' level newSt'') ([],Nothing) sL
                   retAddr <- genNewAddr
                   iAddr   <- genNewAddr
                   tLabel  <- genNewLabel
                   fLabel  <- genNewLabel
                   lLabel  <- genNewLabel
                   return (code1 ++ [makeStmt label1 $ IPARAM(0,1,forExprAddr), makeStmt' $ ICALLR(retAddr, "size", 1)] 
                     ++ [makeStmt' $ ICOPY(iAddr, IINT 0), makeStmt (Just [lLabel]) $ ILT_JUMP(iAddr, retAddr, tLabel)] 
                     ++ [makeStmt' $ IUNCOND_JUMP(fLabel)]
                     ++ [makeStmt  (Just [tLabel]) $ IARRAY_READ(forVarId,forExprAddr,iAddr)] ++ code2
                     ++ [makeStmt label2 $IPLUS(iAddr,iAddr,IINT 1)]
                     ++ [makeStmt' $ IUNCOND_JUMP(lLabel)], Just [fLabel]) 
      _ -> error "translating fail! "
    where insertForStmtId level st vId' ty =
            case look_up st vId' of
              NO_FOUND ->
                let newSt = insert st $ VARIABLE(vId', ty) in
                  return (IID vId, newSt)
              _ -> 
                do
                  newVar <- genNewAddr
                  let newSt = insert st $ SUBST(vId' ,show newVar, ty)
                  return (newVar, newSt)
 
transStmt level st (IfStmt {bExpr=bE, thenStmts=tS, elseStmts=eS, stmtSrcPos=sP}) =
  case eS of
    Just eS' ->
      do 
        tLabel <- genNewLabel
        fLabel <- genNewLabel
        eLabel <- genNewLabel
        code <- transCond level st tLabel fLabel bE
        (code2, label2) <- transStmt level st tS
        (code3, label3) <- transStmt level st eS'
        return (code ++ addLabelToStmtList (Just [tLabel]) code2 ++ [makeStmt' $ IUNCOND_JUMP(eLabel)] 
                ++ addLabelToStmtList (Just [fLabel]) code3, (Just [eLabel]) `mappend` label2 `mappend` label3 ) 
    Nothing ->
      do
        tLabel <- genNewLabel
        fLabel <- genNewLabel
        code <- transCond level st tLabel fLabel bE
        (code2, label2) <- transStmt level st tS
        return (code ++ addLabelToStmtList (Just [tLabel]) code2, (Just [fLabel]) `mappend` label2) 
              
transStmt level st (ReturnStmt {rExpr=rE, stmtSrcPos=sP}) =
  case rE of
    Just e -> 
      if isAddr e
        then return ([makeStmt' $ IRETURN(Just $ getAddr st e)], Nothing)
        else do
               newAddr <- genNewAddr
               (code, label) <- transExpr level st newAddr e
               return (code ++ [makeStmt label $ IRETURN(Just newAddr)], Nothing)

    Nothing -> return ( [makeStmt' $ IRETURN(Nothing) ], Nothing)


transStmt level st (WhileStmt {whileBoolExpr=bE, whileStmts=BlockStmt{stmtList=sL, stmtSrcPos=_}, stmtSrcPos=sP}) =
  do 
    tLabel <- genNewLabel
    fLabel <- genNewLabel
    wLabel <- genNewLabel
    code1 <- transCond level st tLabel fLabel bE
    (code2, label) <- foldM (transStmt' level st) ([],Nothing) sL
    return ( addLabelToStmtList (Just [wLabel]) code1 ++ addLabelToStmtList (Just [tLabel]) code2 
      ++ [makeStmt label $ IUNCOND_JUMP(wLabel)], Just [fLabel]) 

-- 
transStmt' :: Int -> ST -> ([IStmt], Maybe [ILabel]) -> Stmt -> IO([IStmt], Maybe [ILabel])     
transStmt' level st acc stmt =
  do
    (code, label) <- transStmt level st stmt
    return (fst acc ++ addLabelToStmtList (snd acc) code, label)

---
--- Translate expression list into IR Statement List
---
transExprs :: Int -> ST -> [Expr] -> IO( ([IStmt], Maybe [ILabel]) , [IAddr])
transExprs level st exprList =
  do
    foldM transExprWithNewAddr (([],Nothing),[]) exprList
    where transExprWithNewAddr acc expr =
            if isAddr expr 
              then return ((fst acc), (snd acc) ++ [(getAddr st expr)])
              else do
                     let preLabel     = snd $ fst acc
                     let preStmtList  = fst $ fst acc
                     newAddr    <- genNewAddr
                     (codeL, label) <- transExpr level st newAddr expr
                     return ((preStmtList ++ (addLabelToStmtList preLabel codeL), label), (snd acc) ++ [newAddr])


--
-- Translate expression into IR
--  
transExpr :: Int -> ST -> IAddr -> Expr ->  IO ([IStmt], Maybe [ILabel])
transExpr level st addr (infixE@InfixExpr{op=o, inFixLeftExpr=lE, inFixRightExpr=rE, exprSrcPos=sP}) =
  if (o == "&&" || o == "||" || isRelOp o) 
    then do
           tLabel <- genNewLabel
           fLabel <- genNewLabel
           code <- transCond level st tLabel fLabel infixE
           return ( [makeStmt' $ ICOPY(addr, IBOOL True)] ++ code ++ [(makeStmt (Just [fLabel]) $ ICOPY(addr, IBOOL False))], Just [tLabel])
  
    else if isAddr lE 
           then if isAddr rE 
                  then 
                    case o of 
                      "+" -> 
                        do lType <- getTackTypeFromExpr level st lE
                           rType <- getTackTypeFromExpr level st rE
                           if ((lType == TK_STRING) && (rType == TK_STRING)) 
                             then return ([makeStmt' $ IPARAM(0,2,getAddr st lE), makeStmt' $ IPARAM(1,2,getAddr st rE)] 
                                    ++ [makeStmt' $ ICALLR(addr,"append",2)],Nothing) 
                             else if ((lType == TK_STRING) && (rType == TK_INT)) 
                                  then do
                                         castNewAddr <- genNewAddr
                                         return ([makeStmt' $ ICAST(castNewAddr, getAddr st rE, TK_STRING)]
                                           ++ [makeStmt' $ IPARAM(0,2,getAddr st lE), makeStmt' $ IPARAM(1,2,castNewAddr)] 
                                           ++ [makeStmt' $ ICALLR(addr,"append",2)], Nothing)
                                  else if ((lType == TK_INT) && (rType == TK_STRING)) 
                                       then do
                                              castNewAddr <- genNewAddr
                                              return ([makeStmt' $ ICAST(castNewAddr, getAddr st lE, TK_STRING)]
                                                ++ [makeStmt' $ IPARAM(0,2, castNewAddr), makeStmt' $ IPARAM(1,2, getAddr st rE)] 
                                                ++ [makeStmt' $ ICALLR(addr,"append",2)], Nothing)
                                       else return ([makeStmt' $ IPLUS (addr, getAddr st lE, getAddr st rE)], Nothing)
                      "-" -> return ([makeStmt' $ IMINUS(addr, getAddr st lE, getAddr st rE)], Nothing)
                      "/" -> return ([makeStmt' $ IDIV  (addr, getAddr st lE, getAddr st rE)], Nothing)
                      "*" -> return ([makeStmt' $ ITIMES(addr, getAddr st lE, getAddr st rE)], Nothing)
                      "%" -> return ([makeStmt' $ IMOD  (addr, getAddr st lE, getAddr st rE)], Nothing)
                  else do
                         newRAddr   <- genNewAddr
                         (rCode, label) <- transExpr level st newRAddr rE
                         case o of 
                           "+" -> 
                             do lType <- getTackTypeFromExpr level st lE
                                rType <- getTackTypeFromExpr level st rE
                                if ((lType == TK_STRING) && (rType == TK_STRING)) 
                                  then return (rCode ++ [makeStmt label $ IPARAM(0,2, getAddr st lE)] 
                                         ++ [makeStmt' $ IPARAM(1,2, newRAddr), makeStmt' $ ICALLR(addr, "append",2)], Nothing)
                                  else if ((lType == TK_STRING) && (rType == TK_INT)) 
                                         then do
                                                castNewAddr <- genNewAddr
                                                return ( rCode ++ [makeStmt label $ ICAST(castNewAddr,newRAddr, TK_STRING)]
                                                  ++ [makeStmt' $ IPARAM(0,2,getAddr st lE), makeStmt' $ IPARAM(1,2, castNewAddr)]
                                                  ++ [makeStmt' $ ICALLR(addr, "append",2)], Nothing)
                                         else if ((lType == TK_INT) && (rType == TK_STRING))
                                                then do
                                                       castNewAddr <- genNewAddr
                                                       return (rCode ++ [makeStmt label $ ICAST(castNewAddr,getAddr st lE, TK_STRING)]
                                                         ++  [makeStmt' $ IPARAM(0,2,castNewAddr), makeStmt' $ IPARAM(1,2, newRAddr)]
                                                         ++ [makeStmt' $ ICALLR(addr, "append",2)], Nothing)
                                                else return (rCode ++ [makeStmt label (IPLUS (addr, getAddr st lE, newRAddr))], Nothing)
                           "-" -> return (rCode ++ [makeStmt label (IMINUS(addr, getAddr st lE, newRAddr))], Nothing)
                           "/" -> return (rCode ++ [makeStmt label (IDIV  (addr, getAddr st lE, newRAddr))], Nothing)
                           "*" -> return (rCode ++ [makeStmt label (ITIMES(addr, getAddr st lE, newRAddr))], Nothing)
                           "%" -> return (rCode ++ [makeStmt label (IMOD  (addr, getAddr st lE, newRAddr))], Nothing)
           else if isAddr rE 
                  then do
                         newLAddr   <- genNewAddr
                         (lCode, label) <- transExpr level st newLAddr lE
                         case o of 
                           "+" -> 
                             do lType <- getTackTypeFromExpr level st lE
                                rType <- getTackTypeFromExpr level st rE
                                if ((lType == TK_STRING) && (rType == TK_STRING)) 
                                  then return (lCode ++ [makeStmt label $ IPARAM(0,2, newLAddr)] 
                                         ++ [makeStmt' $ IPARAM(1,2, getAddr st rE), makeStmt' $ ICALLR(addr, "append",2)], Nothing)
                                  else if ((lType == TK_STRING) && (rType == TK_INT))
                                         then do
                                                castNewAddr <- genNewAddr
                                                return (lCode ++ [makeStmt label $ ICAST(castNewAddr, getAddr st rE, TK_STRING)]
                                                  ++ [makeStmt' $ IPARAM(0,2,newLAddr), makeStmt' $ IPARAM(1,2, castNewAddr)]
                                                  ++ [makeStmt' $ ICALLR(addr, "append",2)], Nothing)
                                         else if ((lType == TK_INT) && (rType == TK_STRING))
	                                            then do
                                                       castNewAddr <- genNewAddr
                                                       return  (lCode ++ [makeStmt label $ ICAST(castNewAddr, newLAddr, TK_STRING)]
                                                         ++ [makeStmt' $ IPARAM(0,2,castNewAddr), makeStmt' $ IPARAM(1,2, getAddr  st rE)]
                                                         ++ [makeStmt' $ ICALLR(addr, "append",2)], Nothing)
                                                else return (lCode ++ [makeStmt label (IPLUS (addr, newLAddr, getAddr st rE))], Nothing)
                           "-" -> return (lCode ++ [makeStmt label (IMINUS(addr, newLAddr, getAddr st rE))], Nothing)
                           "/" -> return (lCode ++ [makeStmt label (IDIV  (addr, newLAddr, getAddr st rE))], Nothing)
                           "*" -> return (lCode ++ [makeStmt label (ITIMES(addr, newLAddr, getAddr st rE))], Nothing)
                           "%" -> return (lCode ++ [makeStmt label (IMOD(addr, newLAddr, getAddr st rE))], Nothing)
                  else do
                         newLAddr <- genNewAddr
                         newRAddr <- genNewAddr
                         (lCode, label1) <- transExpr level st newLAddr lE
                         (rCode, label2) <- transExpr level st newRAddr rE
                         case o of
                           "+" ->
                             do lType <- getTackTypeFromExpr level st lE
                                rType <- getTackTypeFromExpr level st rE
                                if ((lType == TK_STRING) && (rType == TK_STRING)) 
                                  then return ( lCode ++(addLabelToStmtList label1 rCode) 
                                         ++ [makeStmt label2 $ IPARAM(0,2,newLAddr), makeStmt' $ IPARAM(1,2,newRAddr)]
                                         ++ [makeStmt' $ ICALLR(addr,"append",2)], Nothing)
                                  else if ((lType == TK_STRING) && (rType == TK_INT)) 
                                         then do
                                                castNewAddr <- genNewAddr
                                                return ( lCode ++(addLabelToStmtList label1 rCode)
                                                  ++ [makeStmt' $ ICAST(castNewAddr, newRAddr, TK_STRING)]
                                                  ++ [makeStmt' $ IPARAM(0,2, newLAddr), makeStmt' $ IPARAM(1,2, castNewAddr)]
                                                  ++ [makeStmt' $ ICALLR(addr, "append",2)], Nothing)
                                         else if ((lType == TK_INT) && (rType == TK_STRING))
                                                then do 
                                                       castNewAddr <- genNewAddr
                                                       return ( lCode ++(addLabelToStmtList label1 rCode)
                                                         ++ [makeStmt' $ ICAST(castNewAddr, newLAddr, TK_STRING)]
                                                         ++  [makeStmt' $ IPARAM(0,2, castNewAddr), makeStmt' $ IPARAM(1,2, newRAddr)] 
                                                         ++  [makeStmt' $ ICALLR(addr,"append",2)], Nothing)
                                                else return ( lCode ++(addLabelToStmtList label1 rCode) 
                                                       ++ [makeStmt label2 $ IPLUS (addr, newLAddr, newRAddr)], Nothing)
                           "-" -> return ( lCode ++(addLabelToStmtList label1 rCode)
                                    ++ [makeStmt label2 $ IMINUS(addr, newLAddr, newRAddr)], Nothing)
                           "/" -> return ( lCode ++(addLabelToStmtList label1 rCode)
                                    ++ [makeStmt label2 $ IDIV  (addr, newLAddr, newRAddr)], Nothing)
                           "*" -> return ( lCode ++(addLabelToStmtList label1 rCode) 
                                    ++ [makeStmt label2 $ ITIMES(addr, newLAddr, newRAddr)], Nothing)
                           "%" -> return ( lCode ++(addLabelToStmtList label1 rCode)  
                                    ++ [makeStmt label2 $ IMOD  (addr, newLAddr, newRAddr)], Nothing)
   
transExpr level st addr (prefix@PrefixExpr{op=o,prefixExpr=pE,exprSrcPos=sP}) =
  if o == "!" 
    then do
           tLabel <- genNewLabel
           fLabel <- genNewLabel 
           code <- transCond level st tLabel fLabel prefix
           return ( [makeStmt' $ ICOPY(addr, IBOOL True)] ++ code ++ [makeStmt (Just [fLabel]) $ ICOPY(addr, IBOOL False)], Just [tLabel])
    else if isAddr pE 
           then return ([makeStmt' $ IPMINUS(addr, getAddr st pE)], Nothing)
           else do
                  newAddr <- genNewAddr
                  (code, label) <- transExpr level st newAddr pE
                  return (code ++ [makeStmt label $ IPMINUS(addr, newAddr)], Nothing)
        
transExpr level st addr (CallExpr {fNameExpr=fN,argExprs=aE,exprSrcPos=sP}) =
  do
    ((pCode, label), argL)  <- transExprs level st aE 
    let arity = length argL 
    case fN of
      FunId{funIdName=fId,exprSrcPos=_} -> return ( pCode ++ (addLabelToStmtList label (foldl (genIParamCode arity) [] argL)) ++ [makeStmt' $ ICALLR(addr, fId, arity)], Nothing)
      _ -> error "error!"
	  
transExpr level st addr (CastExpr {castExpr=cE,castType=cT,exprSrcPos=sP}) =
  if isAddr cE
    then do
           cType <- getTackType cT
           return ([makeStmt Nothing $ ICAST(addr, getAddr st cE, cType)], Nothing)
    else do
           cType <- getTackType cT
           newAddr   <- genNewAddr
           (code, label) <- transExpr level st newAddr cE
           return (code ++ [makeStmt label $ ICAST(addr, newAddr, cType)], Nothing)
       
transExpr level st addr (FieldExpr{fieldExpr=fE, fieldExprId=fId,exprSrcPos=_}) =
  if isAddr fE 
    then return ([makeStmt' $ IRECORD_READ(addr, getAddr st fE, fieldIdName fId)], Nothing)
    else do
           newAddr   <- genNewAddr
           (code, label) <- transExpr level st newAddr fE
           return (code ++ [makeStmt label $ IRECORD_READ(addr, newAddr, fieldIdName fId)], Nothing)
 
transExpr level st addr (SubscriptExpr{sExpr=sE, subscript=subE,exprSrcPos=sP}) =
  if isAddr sE 
    then if isAddr subE 
           then return $ ( [makeStmt' $ IARRAY_READ(addr, getAddr st sE, getAddr st subE)], Nothing)
           else do
                  newAddr <- genNewAddr
                  (code, label) <- transExpr level st newAddr subE
                  return ( code ++ [makeStmt label $ IARRAY_READ(addr, getAddr st sE, newAddr)], Nothing)
    else if isAddr subE
           then do
                  newAddr <- genNewAddr
                  (code, label) <- transExpr level st newAddr sE
                  return ( code ++ [makeStmt label $ IARRAY_READ(addr, newAddr, getAddr st subE)], Nothing)
           else do
                  newAddr1 <- genNewAddr
                  (code1, label1) <- transExpr level st newAddr1 sE
                  newAddr2 <- genNewAddr
                  (code2, label2) <- transExpr level st newAddr2 subE
                  return (code1 ++ (addLabelToStmtList label1 code2) ++ [makeStmt label2 $ IARRAY_READ(addr, newAddr1, newAddr2)], Nothing)
 
transExpr level st addr (ParenExpr{pExpr=pE,exprSrcPos=sP}) =
  transExpr level st addr pE

transExpr level st addr (ArrayLit{arrayExprs=aEL,exprSrcPos=_}) =
  do
    ((codeL, label), elemL)  <- transExprs level st aEL
    let arity = length aEL
    if arity == 0 
	  then return (codeL ++ (addLabelToStmtList label $ fst $ foldl (genIArrayWriteCode addr) ([],0) elemL), Nothing)
      else do
             tType <- getTackTypeFromExpr level st $ head aEL
             return ([makeStmt' $ IPARAM(0,2,ISIZEOF tType), makeStmt' $ IPARAM(1,2,IINT arity), makeStmt' $ ICALLR(addr,"newArray",arity)]
               ++ codeL ++ (addLabelToStmtList label $ fst $ foldl (genIArrayWriteCode addr) ([],0) elemL), Nothing)

transExpr level st addr (recordT@RecordLit{recordFieldLits=rFL, exprSrcPos=_}) =
  do
    ((codeL, label), argL) <-  transFieldLits level st rFL
    eType <- getTackTypeFromExpr level st recordT
    if length argL == 0 
      then return (codeL ++ (addLabelToStmtList label $ foldl (genIRecordWriteCode addr) [] argL), Nothing)
      else return ( [makeStmt' $ IPARAM(0,1, ISIZEOF eType), makeStmt' $ ICALLR(addr, "newRecord", 1)]
             ++ codeL ++ (addLabelToStmtList label $ foldl (genIRecordWriteCode addr) [] argL), Nothing)

transExpr level st addr expr =
  return ([], Nothing)

transCond :: Int -> ST -> ILabel -> ILabel -> Expr -> IO [IStmt]
transCond level st tLabel fLabel expr =
  case expr of
    infixOpt@(InfixExpr{op=o, inFixLeftExpr=lE, inFixRightExpr=rE, exprSrcPos=_}) ->
      case o of 
        "&&" ->
          do
            label <- genNewLabel
            code1 <- transCond level st label fLabel lE
            code2 <- transCond level st tLabel fLabel rE         
            return $ code1 ++ (addLabelToStmtList (Just [label]) code2)
        "||" ->
          do
            label <- genNewLabel
            code1 <- transCond level st tLabel label lE
            code2 <- transCond level st tLabel fLabel rE         
            return $ code1 ++ (addLabelToStmtList (Just [label]) code2)
        _  -> 
          if isRelOp o 
            then if isAddr lE 
                   then if isAddr rE 
                          then case o of 
                                 "==" -> return $ [makeStmt' $ IEQ_JUMP(getAddr st lE, getAddr st rE, tLabel)] 
                                           ++ [makeStmt' $  IUNCOND_JUMP(fLabel)] 
                                 "!=" -> return $ [makeStmt' $ INEQ_JUMP(getAddr st lE, getAddr st rE, tLabel)] 
                                           ++ [ makeStmt' $  IUNCOND_JUMP(fLabel)] 
                                 ">"  -> return $ [makeStmt' $ IGT_JUMP(getAddr st lE, getAddr st rE, tLabel)] 
                                           ++ [makeStmt' $  IUNCOND_JUMP(fLabel)] 
                                 ">=" -> return $ [makeStmt' $ IGE_JUMP(getAddr st lE, getAddr st rE, tLabel)] 
                                           ++ [makeStmt' $  IUNCOND_JUMP(fLabel)] 
                                 "<"  -> return $ [makeStmt' $ ILT_JUMP(getAddr st lE, getAddr st rE, tLabel)] 
                                           ++ [makeStmt' $  IUNCOND_JUMP(fLabel)] 
                                 "<=" -> return $ [makeStmt' $ ILE_JUMP(getAddr st lE, getAddr st rE, tLabel)] 
                                           ++ [makeStmt' $  IUNCOND_JUMP(fLabel)] 
                          else do
                                 newAddr <- genNewAddr
                                 (code, label) <- transExpr level st newAddr rE
                                 case o of 
                                   "==" -> return $ code ++ [makeStmt label $ IEQ_JUMP(getAddr st lE, newAddr, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                                   "!=" -> return $ code ++ [makeStmt label $ INEQ_JUMP(getAddr st lE, newAddr, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                                   ">"  -> return $ code ++ [makeStmt label $ IGT_JUMP(getAddr st lE, newAddr, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                                   ">=" -> return $ code ++ [makeStmt label $ IGE_JUMP(getAddr st lE, newAddr, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                                   "<"  -> return $ code ++ [makeStmt label $ ILT_JUMP(getAddr st lE, newAddr, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                                   "<=" -> return $ code ++ [makeStmt label $ ILE_JUMP(getAddr st lE, newAddr, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                   else if isAddr rE
                          then do 
                                 newAddr <- genNewAddr
                                 (code, label) <- transExpr level st newAddr lE
                                 case o of
                                   "==" -> return $ code ++ [makeStmt label $ IEQ_JUMP(newAddr, getAddr st rE, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                                   "!=" -> return $ code ++ [makeStmt label $ INEQ_JUMP(newAddr, getAddr st rE, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                                   ">"  -> return $ code ++ [makeStmt label $ IGT_JUMP(newAddr, getAddr st rE, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                                   ">=" -> return $ code ++ [makeStmt label $ IGE_JUMP(newAddr, getAddr st rE, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                                   "<"  -> return $ code ++ [makeStmt label $ ILT_JUMP(newAddr, getAddr st rE, tLabel)]  
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                                   "<=" -> return $ code ++ [makeStmt label $ ILE_JUMP(newAddr, getAddr st rE, tLabel)] 
                                             ++ [makeStmt' $ IUNCOND_JUMP(fLabel)] 
                          else do
                                 newAddr1 <- genNewAddr
                                 newAddr2 <- genNewAddr
                                 (code1, label1) <- transExpr level st newAddr1 lE
                                 (code2, label2) <- transExpr level st newAddr2 rE
                                 case o of
                                   "==" -> return $ code1 ++ (addLabelToStmtList label1 code2) 
                                             ++ [makeStmt label2 $ IEQ_JUMP(newAddr1, newAddr2, tLabel)] 
                                             ++ [ makeStmt' $ IUNCOND_JUMP(fLabel)]          
                                   "!=" -> return $ code1 ++ (addLabelToStmtList label1 code2) 
                                             ++ [makeStmt label2 $ INEQ_JUMP(newAddr1, newAddr2, tLabel)] 
                                             ++ [ makeStmt' $ IUNCOND_JUMP(fLabel)]          
                                   ">" -> return $ code1 ++ (addLabelToStmtList label1 code2) 
                                             ++ [makeStmt label2 $ IGT_JUMP(newAddr1, newAddr2, tLabel)] 
                                             ++ [ makeStmt' $ IUNCOND_JUMP(fLabel)]          
                                   ">="  -> return $ code1 ++ (addLabelToStmtList label1 code2) 
                                             ++ [makeStmt label2 $ IGE_JUMP(newAddr1, newAddr2, tLabel)] 
                                             ++ [ makeStmt' $ IUNCOND_JUMP(fLabel)]          
                                   "<" -> return $ code1 ++ (addLabelToStmtList label1 code2) 
                                            ++ [makeStmt label2 $ ILT_JUMP(newAddr1, newAddr2, tLabel)] 
                                            ++ [ makeStmt' $ IUNCOND_JUMP(fLabel)]          
                                   "<=" -> return $ code1 ++ (addLabelToStmtList label1 code2) 
                                            ++ [makeStmt label2 $ ILE_JUMP(newAddr1, newAddr2, tLabel)] 
                                            ++ [ makeStmt' $ IUNCOND_JUMP(fLabel)]          
            else do
                   newAddr <- genNewAddr
                   (code, label) <- transExpr level st newAddr infixOpt
                   return $ code ++ [makeStmt label $ ITRUE_JUMP(newAddr, tLabel), makeStmt' $ IUNCOND_JUMP(fLabel)]
    
    PrefixExpr{op="!",prefixExpr=pE,exprSrcPos=sP} ->
      transCond level st fLabel tLabel pE

    BoolLit{boolValue=bV,exprSrcPos=sP} ->
      if bV == True
        then return [makeStmt' $ IUNCOND_JUMP(tLabel)]
        else return [makeStmt' $ IUNCOND_JUMP(fLabel)]
    _ ->
      if isAddr expr 
        then return [makeStmt' $ ITRUE_JUMP(getAddr st expr, tLabel), makeStmt' $ IUNCOND_JUMP(fLabel)]
        else do
               newAddr <- genNewAddr
               (code, label) <- transExpr level st newAddr expr
               return $ code ++ [makeStmt label $ ITRUE_JUMP(newAddr, tLabel), makeStmt' $ IUNCOND_JUMP(fLabel)]


insertFun :: Int -> ST -> FunDef -> IO ST
insertFun level st (FunDef {funId=FunId {funIdName=fname, exprSrcPos=_}, funType=FunType{recordType=argT,retType=retT,typeSrcPos=_}, bStmt=_, funSrcPos=_}) =
  do
    argT' <- getTackType argT
    retT' <- getTackType retT
    return $ insert st (FUNCTION (fname, argT', retT'))

insertSymbol :: Int -> ST -> Stmt -> IO ST
insertSymbol level st (VarDef {varId=vId, varExpr=ve, stmtSrcPos=_}) =
  do
    exprType <- getTackTypeFromExpr level st ve
    let varId = varIdName vId
    case look_up st varId of
      NO_FOUND ->
        return $ insert st (VARIABLE (varId, exprType))
      _ ->
        do
          newAddr <- genNewAddr
          return $ insert st (SUBST(varId, show newAddr, exprType))

insertSymbol level st (WhileStmt {whileBoolExpr=bE, whileStmts=s, stmtSrcPos=sP}) =
  insertSymbol level st s

insertSymbol leve st _ = return st


insertArgument:: Int -> ST -> Type -> IO ST
insertArgument level st (FieldType {fieldId=fId, fieldType=fType, typeSrcPos=sP}) =
  do
    fT     <- getTackType fType
    return $ insert st (VARIABLE(fieldIdName fId, fT))

insertArgument level st _ = return st


--- 
genIParamCode :: Int -> [IStmt] -> IAddr -> [IStmt]
genIParamCode arity acc arg =
         acc ++ [makeStmt' $ IPARAM(length acc, arity, arg)] 

genIArrayWriteCode :: IAddr -> ([IStmt], IIndex) -> IAddr -> ([IStmt], IIndex)
genIArrayWriteCode addr acc rAddr =
  let preStmtList = fst acc in
    let arrayIndex    = snd acc in
      (preStmtList ++ [makeStmt' $ IARRAY_WRITE(addr, IINT arrayIndex, rAddr)], arrayIndex +1 )

genIRecordWriteCode :: IAddr -> [IStmt] -> (IId, IAddr) -> [IStmt] 
genIRecordWriteCode addr acc rTuple =
  acc ++ [makeStmt' $ IRECORD_WRITE(addr, fst rTuple, snd rTuple)]

