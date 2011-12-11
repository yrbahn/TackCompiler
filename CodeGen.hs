module CodeGen where
import IR
import ASM
import Control.Monad

codeGenProg :: IProg -> ASMTranslator IO AsmProg
codeGenProg (IPROG funList) =
  do
    funL <- mapM codeGenFun funList
    return $ AsmProg IntelSyntax funL

codeGenFun :: IFun -> ASMTranslator IO Function
codeGenFun (IFUN(symT, name, ty, stmtList)) =
  do
    let name' = filter (/='"') name 
    let (symbolList, localCnt) = foldl makeSymbol ([],0) stmtList 
    let asmState = ASMState { st = symT, symbols = symbolList }      
    putASMState asmState
    instList <- foldM  codeGenStmt' [] stmtList
    return $ Function (TextSection [Type name' ("."++ name'), Def name'] instList) Nothing
    where makeSymbol symbolInfo@(symbolList, cnt)  stmt = 
            let ISTMT(_,inst) = stmt in
              case inst of
                ICOPY(l,r) ->
                   case l of 
                     IID str ->
                       case lookup str symbolList of
                         Nothing -> ((str,8*(cnt+1)):symbolList, cnt+1) 
                         _ -> symbolInfo
                     _ -> symbolInfo
                _ -> symbolInfo

          codeGenStmt' preStmt stmt =
            do stmt' <- codeGenStmt stmt
               return $ preStmt ++ stmt'                                

codeGenStmt :: IStmt -> ASMTranslator IO [Instruction]
codeGenStmt (ISTMT(labelL, instr)) =
  do
    instList <- codeGenInst instr
    return $ (map (\s -> Define s) labelL) ++ instList 

codeGenInst :: IInst -> ASMTranslator IO [Instruction]
codeGenInst (ICOPY(l,r)) = return []

-- codeGenInst (IPLUS(l,r1,r2)) =	
-- codeGenInst (IMINUS(l,r1,r2)) =
-- codeGenInst (ITIMES(l,r1,r2)) =
-- codeGenInst (IDIV(l,r1,r2)) =
-- codeGenInst (IMOD(l,r1,r2)) =
-- codeGenInst (IPMINUS(l,r)) =
-- codeGenInst (ICAST(l,r,ty)) =
-- codeGenInst (IUNCOND_JUMP(l)) =
-- codeGenInst (ITRUE_JUMP(b,l)) =
-- codeGenInst (IFALSE_JUMP(b,l)) =
-- codeGenInst (IEQ_JUMP(a,b,l)) =
-- codeGenInst (INEQ_JUMP(a,b,l)) =
-- codeGenInst (IGT_JUMP(a,b,l)) =
-- codeGenInst (IGE_JUMP(a,b,l)) =
-- codeGenInst (ILT_JUMP(a,b,l)) =
-- codeGenInst (ILE_JUMP(a,b,l)) =
-- codeGenInst (IPARAM(index,arity,addr)) =
-- codeGenInst (ICALL(name,arity)) =
-- codeGenInst (ICALLR(l,name,arity)) =
-- codeGenInst (IRETURN(addr)) =
-- codeGenInst (IARRAY_READ(l,array,index)) =
-- codeGenInst (IARRAY_WRITE(array,index,addr)) =
-- codeGenInst (IRECORD_READ(addr,record,field)) =
-- codeGenInst (IRECORD_WRITE(record,field,addr)) =
codeGenInst _ = return []	

getOperand :: Addr -> ASMTranslator IO Operand
getOperand (IID i) = 
  do s <- getASMState 
     case look_up (st s) i of
       I_VARIABLE(_,offset,_) -> 
         if offset > 0 
           then return $ spil offset
           else return $ 
       NO_FOUND -> 
         do let (offset, newSt) = insert (st s) (VARIABLE(i,TK_NULL)) 
            putASMState $ s { st = newSt }
            return $ spil offset
       _ -> error "error"
    where spil os = Mem $ show RBP ++ "-" ++ show $ os*8  

getOperand (IBOOL b) =
  case b of
    True -> return $ ImmNum 1
    False -> return $ ImmNum 0

getOperand (IINT i) =
  return $ ImmNum i

getOperand (IString s) =
  return $ LabelOperand s


getOperand INULL =
  return $ ImmNum 0


getOperand (ISIZEOF TACK_TYPE) =
  case TACK_TYPE of
    TK_INT -> return $ ImmNum 8
    _      -> return $ ImmNum 8 	
