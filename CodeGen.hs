module CodeGen where
import IR
import ASM
import Control.Monad
import SymbolTable
import SymbolTypes

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
    let frameSize = localCnt * 8
    let proglogue = if frameSize == 0 
                      then [Push (Res RBP), Mov (Res RBP) (Res RSP)]
                      else [Push (Res RBP), Mov (Res RBP) (Res RSP), Sub (Res RSP) (ImmNum frameSize)]
    let endlogue  = [Def $ name' ++ ".end", Size name' (".-" ++ name')]
    let asmState = ASMState { funName=name', st = symT, symbols = symbolList, stringList=[] }      
    putASMState asmState
    instList <- foldM  codeGenStmt' [] stmtList
    return $ Function (TextSection [Global name', Type name' ("."++ name')]  ((Define name):(proglogue ++ instList)) endlogue) Nothing
    where makeSymbol symbolInfo@(symbolList, cnt)  stmt = 
            let ISTMT(_,inst) = stmt in
              case getLeftAddr inst of
                Just i ->
                  case i of 
                    IID str ->
                      case lookup str symbolList of
                        Nothing -> 
                          case look_up symT str of 
                            I_VARIABLE(_, offset,_) ->
                              if offset > 0 -- not argumnet
                                then ((str,8*(cnt+1)):symbolList, cnt+1) 
                                else symbolInfo
                            _ -> ((str,8*(cnt+1)):symbolList, cnt+1)
                        _ -> symbolInfo
                    _ -> symbolInfo
                Nothing -> symbolInfo
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
codeGenInst (IPARAM(index,arity,addr)) =
  do
    argment <- getOperand addr
    proglogue <- when (index == 0) ([Sub (Res RSP) (ImmNum arity*8)])
    return $ Mov 
                   
-- codeGenInst (ICALL(name,arity)) =
-- codeGenInst (ICALLR(l,name,arity)) =
codeGenInst (IRETURN(addr)) =
  case addr of
    Nothing -> return [Mov (Res RSP) (Res RBP), Pop (Res RBP), Ret] 
    Just addr' -> 
      do
        result <- getOperand addr'
        return [Mov (Res resultReg) result, Mov (Res RSP) (Res RBP), Pop (Res RBP), Ret]


-- codeGenInst (IARRAY_READ(l,array,index)) =
-- codeGenInst (IARRAY_WRITE(array,index,addr)) =
-- codeGenInst (IRECORD_READ(addr,record,field)) =
-- codeGenInst (IRECORD_WRITE(record,field,addr)) =
codeGenInst _ = return []	

getOperand :: IAddr -> ASMTranslator IO Operand
getOperand (IID i) = 
  do s <- getASMState 
     let symbolL = symbols s
     case lookup i symbolL of
       Just offset -> return $ spil offset
       Nothing -> -- register
         case look_up (st s) i of
           I_VARIABLE(_,offset,_) ->
             if offset < 1 
               then return $ getArgRegOrMem offset
               else error "error"
           _ -> error "error"
    where spil os = Mem $ (show RBP) ++ "-" ++ (show $ os*8)
          getArgRegOrMem os = 
            let pOffset = -1 * os in
              if pOffset > 5 
                then Res $ paramRegs !! pOffset
                else Mem $ (show RSP) ++ "+" ++ (show $ 8*(pOffset+1-7))

getOperand (IBOOL b) =
  case b of
    True -> return $ ImmNum 1
    False -> return $ ImmNum 0

getOperand (IINT i) =
  return $ ImmNum i

getOperand (ISTRING s) =
  do 
    state <- getASMState
    let stringCnt = length (stringList state)
    let stringLabel = (funName state) ++ ".S_" ++ (show stringCnt) 
    putASMState state { stringList = (stringLabel,s):(stringList state) }
    return $ LabelOperand stringLabel


getOperand INULL =
  return $ ImmNum 0


getOperand (ISIZEOF ty) =
  case ty of
    TK_INT -> return $ ImmNum 8
    _      -> return $ ImmNum 8 	
