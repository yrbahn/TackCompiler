module CodeGen where
import IR
import ASM
import Control.Monad
import SymbolTable
import SymbolTypes
import Control.Monad.Trans

--
-- Linux x86_64 code generation
--

codeGenProg :: IProg -> ASMTranslator IO AsmProg
codeGenProg (IPROG funList) =
  do
    funL <- mapM codeGenFun funList
    return $ AsmProg IntelSyntax funL

codeGenFun :: IFun -> ASMTranslator IO Function
codeGenFun (IFUN(symT, name, ty, stmtList)) =
  do
    let name' = filter (/='"') name 
    let argList = getArgsFromCurrentScope name symT
    let argCnt  = length argList
    let (symbolList, localCnt) = foldl makeSymbol (argList,argCnt) stmtList 
    let frameSize = localCnt * 8
    spilArgsCode <- mapM makeSpilArgCode argList
    let proglogue = if frameSize == 0 
                      then [Push (Res RBP), Mov (Res RBP) (Res RSP)]
                      else [Push (Res RBP), Mov (Res RBP) (Res RSP)] ++ (concat spilArgsCode) 
                           ++ [ Sub (Res RSP) (ImmNum frameSize)]
    let endlogue  = [Def $ name' ++ ".end", Size name' (".-" ++ name')]
    --lift $ putStrLn $ show symbolList
    let asmState = ASMState { funName=name', st = symT, symbols = symbolList, stringList=[], freeRegs=freeRegisters }      
    putASMState asmState
    instList <- foldM  codeGenStmt' [] stmtList
    state <- getASMState
    if (length $ stringList state) == 0 
      then  
        return $ Function (TextSection [Global name', Type name' "@function"]  
                                       ((Define name):(proglogue ++ instList)) endlogue) 
                          Nothing
      else 
        return $ Function (TextSection [Global name', Type name' "@function"]  
                                       ((Define name):(proglogue ++ instList)) endlogue) 
                          (Just $ DataSection $ concat  $ map (\s -> [Def $ fst s, Str $ snd s]) (stringList state))
 
    where makeSymbol symbolInfo@(symbolList, cnt)  stmt = 
            let ISTMT(_,inst) = stmt in
              case getLeftAddr inst of
                Just str ->
                  case lookup str symbolList of
                    Nothing -> 
                      ((str, cnt+1):symbolList, cnt+1) 
                    _ -> symbolInfo
                Nothing -> symbolInfo
          codeGenStmt' preStmt stmt =
            do stmt' <- codeGenStmt stmt
               return $ preStmt ++ stmt'                                
          makeSpilArgCode (var, index) =
            do m <-  getMemByIndex (index-1)
               r <- getArgOpByIndex (index-1)
               return  [Mov m r]

codeGenStmt :: IStmt -> ASMTranslator IO [Instruction]
codeGenStmt (ISTMT(labelL, instr)) =
  do
    instList <- codeGenInst instr
    return $ (map (\s -> Define s) labelL) ++ instList 

codeGenInst :: IInst -> ASMTranslator IO [Instruction]
codeGenInst (ICOPY(l,r)) = 
  do (newReg, instL) <- moveToAnyReg r
     leftOp <- getOperand l
     freeRegister newReg
     return $ instL ++ [Mov leftOp newReg]

codeGenInst (IPLUS(l,r1,r2)) =	
  do (newReg1, instL1) <- moveToAnyReg r1
     (newReg2, instL2) <- moveToAnyReg r2
     leftOp <- getOperand l
     let result = instL1 ++ instL2 ++ [Add newReg1 newReg2, Mov leftOp newReg1]
     freeRegister newReg1
     freeRegister newReg2
     return result
       
codeGenInst (IMINUS(l,r1,r2)) =
  do (newReg1, instL1) <- moveToAnyReg r1
     (newReg2, instL2) <- moveToAnyReg r2
     leftOp <- getOperand l
     let result = instL1 ++ instL2 ++ [Sub newReg1 newReg2, Mov leftOp newReg1]
     freeRegister newReg1
     freeRegister newReg2
     return result

codeGenInst (ITIMES(l,r1,r2)) =
  do (newReg1, instL1) <- moveToAnyReg r1
     (newReg2, instL2) <- moveToAnyReg r2
     leftOp <- getOperand l
     let result = instL1 ++ instL2 ++ [IMul newReg1 newReg2, Mov leftOp newReg1]
     freeRegister newReg1
     freeRegister newReg2
     return result

codeGenInst (IDIV(l,r1,r2)) =
  do (raxReg, instL) <- moveToReg r1 RAX
     rdxReg <- allocRegister RDX
     (r2', instL2) <- moveToAnyReg r2  
     leftOp <- getOperand l
     let result = instL ++ [Mov rdxReg raxReg, Sar rdxReg (ImmNum 63)]
                  ++ instL2 ++ [IDiv r2', Mov leftOp raxReg]
     freeRegister raxReg
     freeRegister rdxReg
     freeRegister r2'
     return result

codeGenInst (IMOD(l,r1,r2)) =
  do (raxReg, instL) <- moveToReg r1 RAX
     rdxReg <- allocRegister RDX
     (r2', instL2) <- moveToAnyReg r2  
     leftOp <- getOperand l
     let result = instL ++ [Mov rdxReg raxReg, Sar rdxReg (ImmNum 63)]
                  ++ instL2 ++ [IDiv r2', Mov leftOp rdxReg]
     freeRegister raxReg
     freeRegister rdxReg
     freeRegister r2'
     return result

codeGenInst (IPMINUS(l,r)) =
  do (newReg, instL) <- moveToAnyReg r
     leftOp <- getOperand l
     let result = instL ++ [Neg newReg, Mov leftOp newReg]
     freeRegister newReg
     return result

codeGenInst (ICAST(l,r,ty)) =
  do let addrTy = getAddrType r
     if addrTy == ty 
       then do leftOp <- getOperand l
               (newReg, instL) <- moveToAnyReg r
               let result = instL ++ [Mov leftOp newReg]
               freeRegister newReg
               return result
       else case (addrTy,ty) of
              (TK_BOOL, TK_INT) ->
                do (argReg,instL) <- moveToReg r RDI
                   leftOp <- getOperand l
                   retReg <- allocRegister resultReg
                   let result = instL ++ [Call "bool2int", Mov leftOp retReg]
                   freeRegister argReg
                   freeRegister retReg
                   return result
              (TK_INT,  TK_BOOL) ->
                do (argReg,instL) <- moveToReg r RDI
                   leftOp <- getOperand l
                   retReg <- allocRegister resultReg
                   let result = instL ++ [Call "int2bool", Mov leftOp retReg]
                   freeRegister argReg
                   freeRegister retReg
                   return result
              (TK_INT, TK_STRING) ->
                do (argReg,instL) <- moveToReg r RDI
                   leftOp <- getOperand l
                   retReg <- allocRegister resultReg
                   let result = instL ++ [Call "int2string", Mov leftOp retReg]
                   freeRegister argReg
                   freeRegister retReg
                   return result
              (TK_BOOL, TK_STRING) ->
                do (argReg,instL) <- moveToReg r RDI
                   leftOp <- getOperand l
                   retReg <- allocRegister resultReg
                   let result = instL ++ [Call "bool2string", Mov leftOp retReg]
                   freeRegister argReg
                   freeRegister retReg
                   return result
              (TK_STRING, TK_INT) ->
                do (argReg,instL) <- moveToReg r RDI
                   leftOp <- getOperand l
                   retReg <- allocRegister resultReg
                   let result = instL ++ [Call "string2int", Mov leftOp retReg]
                   freeRegister argReg
                   freeRegister retReg
                   return result
              (TK_STRING, TK_BOOL) ->
                do (argReg,instL) <- moveToReg r RDI
                   leftOp <- getOperand l
                   retReg <- allocRegister resultReg
                   let result = instL ++ [Call "string2bool", Mov leftOp retReg]
                   freeRegister argReg
                   freeRegister retReg
                   return result
              _ -> do leftOp <- getOperand l
                      (newReg, instL) <- moveToAnyReg r                           
                      let result = instL ++ [Mov leftOp newReg]
                      freeRegister newReg
                      return result
    
codeGenInst (IUNCOND_JUMP(l)) =
  return $ [Jmp l]

codeGenInst (ITRUE_JUMP(b,l)) =
  do (newReg, instL) <- moveToAnyReg b
     let result = instL ++ [Cmp newReg (ImmNum 1), Je l]
     freeRegister newReg
     return result

codeGenInst (IFALSE_JUMP(b,l)) = 
  do (newReg, instL) <- moveToAnyReg b
     let result = instL ++ [Cmp newReg (ImmNum 0), Je l]
     freeRegister newReg
     return result

codeGenInst (IEQ_JUMP(a,b,l)) =
  do (newReg1, instL1) <- moveToAnyReg a
     (newReg2, instL2) <- moveToAnyReg b
     let result = instL1 ++ instL2 ++ [Cmp newReg1 newReg2, Je l]
     freeRegister newReg1
     freeRegister newReg2
     return result

 
codeGenInst (INEQ_JUMP(a,b,l)) =
  do (newReg1, instL1) <- moveToAnyReg a
     (newReg2, instL2) <- moveToAnyReg b
     let result = instL1 ++ instL2 ++ [Cmp newReg1 newReg2, Jne l]
     freeRegister newReg1
     freeRegister newReg2
     return result

codeGenInst (IGT_JUMP(a,b,l)) =
  do (newReg1, instL1) <- moveToAnyReg a
     (newReg2, instL2) <- moveToAnyReg b
     let result = instL1 ++ instL2 ++ [Cmp newReg1 newReg2, Jg l]
     freeRegister newReg1
     freeRegister newReg2
     return result

codeGenInst (IGE_JUMP(a,b,l)) =
  do (newReg1, instL1) <- moveToAnyReg a
     (newReg2, instL2) <- moveToAnyReg b
     let result = instL1 ++ instL2 ++ [Cmp newReg1 newReg2, Jge l]
     freeRegister newReg1
     freeRegister newReg2
     return result

codeGenInst (ILT_JUMP(a,b,l)) =
  do (newReg1, instL1) <- moveToAnyReg a
     (newReg2, instL2) <- moveToAnyReg b
     let result = instL1 ++ instL2 ++ [Cmp newReg1 newReg2, Jl l]
     freeRegister newReg1
     freeRegister newReg2
     return result
	
codeGenInst (ILE_JUMP(a,b,l)) =
  do (newReg1, instL1) <- moveToAnyReg a
     (newReg2, instL2) <- moveToAnyReg b
     let result = instL1 ++ instL2 ++ [Cmp newReg1 newReg2, Jle l]
     freeRegister newReg1
     freeRegister newReg2
     return result


codeGenInst (IPARAM(index,arity,addr)) =
  do
    let proglogue = if (index == 0 && arity > 6 ) 
                      then  [Sub (Res RSP) (ImmNum $ arity*8)]
                      else []
    (newReg, instL) <- moveToAnyReg addr
    mem <- getArgOpByIndex index
    let result = proglogue ++ instL ++ [Mov mem newReg]
    freeRegister newReg
    return result

codeGenInst (ICALL(name,arity)) =
  return [Call name]

codeGenInst (ICALLR(l,name,arity)) =
  do retVar <- getOperand l
     rReg <- allocRegister resultReg
     let result = [Call name, Mov retVar rReg]
     freeRegister rReg
     return result

codeGenInst (IRETURN(addr)) =
  case addr of
    Nothing -> return [Mov (Res RSP) (Res RBP), Pop (Res RBP), Ret] 
    Just addr' -> 
      do
        retOp <- getOperand addr'
        retReg <- allocRegister resultReg
        let result = [Mov retReg retOp, Mov (Res RSP) (Res RBP), Pop (Res RBP), Ret]
        freeRegister retReg
        return result

codeGenInst (IARRAY_READ(l,array,index)) =
  do (newReg1, instL1) <- moveToAnyReg array
     (newReg2, instL2) <- moveToAnyReg index
     leftOp  <- getOperand l  
     let result = instL1 ++ [Mov newReg1 (Mem "" $ (show newReg1) ++ "+" ++(show 8)) ]
                  ++ instL2 ++ [Sal newReg2 (ImmNum 3)] 
                  ++ [Add newReg2 newReg1, Mov newReg2 (Mem "" $ show newReg2)]
                  ++ [Mov leftOp newReg2]
     freeRegister newReg1
     freeRegister newReg2
     return result
    	
codeGenInst (IARRAY_WRITE(array,index,addr)) =
  do (newReg1, instL1) <- moveToAnyReg array
     (newReg2, instL2) <- moveToAnyReg index
     (newReg3, instL3) <- moveToAnyReg addr
     let result = instL1 ++ [Mov newReg1 (Mem "" $ (show newReg1) ++ "+" ++(show 8)) ]
                  ++ instL2 ++ [Sal newReg2 (ImmNum 3)] 
                  ++ [Add newReg2 newReg1] ++ instL3 ++ [Mov (Mem "" $ show newReg2) newReg3]     
     freeRegister newReg1
     freeRegister newReg2
     freeRegister newReg3
     return result

codeGenInst (IRECORD_READ(l,b,fId)) = 
  do let (IID _ recordType) = b
     case recordType of
       (TK_RECORD elemType) ->
         do let offset = getFieldOffset recordType fId
            (newReg1, instL1) <- moveToAnyReg b
            tempReg <- allocAnyRegister
            leftOp <- getOperand l
            let result = instL1 ++ [Add newReg1 (ImmNum offset)] 
                         ++ [Mov tempReg (Mem "" $ show newReg1), Mov leftOp tempReg]
            freeRegister newReg1
            freeRegister tempReg
            return result       
       _ -> error "need record type"
	
codeGenInst (IRECORD_WRITE(b,fId,r)) =
  do let (IID _ recordType) = b
     case recordType of
       (TK_RECORD elemType) ->
         do let offset = getFieldOffset recordType fId
            --lift $ putStrLn $ fId ++ (show offset)
            (newReg1, instL1) <- moveToAnyReg b
            (newReg2, instL2) <- moveToAnyReg r
            let result = instL1 ++ [Add newReg1 (ImmNum offset)] 
                         ++ instL2 ++ [Mov (Mem "" $ show newReg1) newReg2]
            freeRegister newReg1     
            freeRegister newReg2  
            return result
       _ -> error "need record type"

getOperand :: IAddr -> ASMTranslator IO Operand
getOperand (IID i _) = 
  do s <- getASMState 
     let symbolL = symbols s
     case lookup i symbolL of
       Just offset -> getMemByIndex (offset-1)
       Nothing -> do lift $ putStrLn $ show symbolL
                     error $ "No such memory: " ++ i

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
    putASMState (state { stringList = (stringLabel,s):(stringList state) })
    return $ LabelOperand stringLabel


getOperand INULL =
  return $ ImmNum 0


getOperand (ISIZEOF ty) =
  return $ ImmNum $ getTypeSize ty


--
--

moveToReg :: IAddr -> Register -> ASMTranslator IO (Operand, [Instruction])
moveToReg addr reg =
  do op   <- getOperand addr
     reg' <- allocRegister reg
     return (reg', [Mov reg' op])

moveToAnyReg :: IAddr -> ASMTranslator IO (Operand, [Instruction])
moveToAnyReg addr =
  do op  <- getOperand addr
     reg <- allocAnyRegister
     return (reg, [Mov reg op])

getArgOpByIndex :: Int -> ASMTranslator IO Operand
getArgOpByIndex index =
  if index < 7
    then return $ Res $ (!!) paramRegs index
    else return $ Mem "" $ (show RSP) ++ "+" ++ (show $ 8*(index+1-7))

	
allocMem :: Int -> ASMTranslator IO Operand
allocMem os = return $ Mem "" $ (show RBP) ++ "-" ++ (show $ os*8)

getMemByIndex :: Int -> ASMTranslator IO Operand
getMemByIndex os = return $ Mem  "" $ (show RBP) ++ "-" ++ (show $ (os+1)*8)

allocRegister :: Register -> ASMTranslator IO Operand
allocRegister reg = 
  do state <- getASMState
     let fRegs = freeRegs state
     if (elem reg fRegs)
       then do putASMState $ state { freeRegs = filter (\s -> s /= reg) fRegs }
               return $ Res reg
       else error "Cannot alloc the register"

freeRegister :: Operand -> ASMTranslator IO ()
freeRegister op =
  case op of
    (Res r) -> do state <- getASMState
                  let fRegs = freeRegs state
                  if (elem r fRegs) 
                    then return ()
                    else putASMState $ state { freeRegs = r:fRegs}
    _ -> return ()

allocAnyRegister :: ASMTranslator IO Operand
allocAnyRegister =
  do state <- getASMState
     if (length $ freeRegs state) == 0 
       then error "cannot alloc any register"
       else do putASMState $ state {freeRegs=tail $ freeRegs state}
               return $ Res $ head $ freeRegs state 

