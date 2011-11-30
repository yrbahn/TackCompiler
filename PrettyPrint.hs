module PrettyPrint (ppAST, indent) where
import AST

-- Pretty print 

ppAST :: AST -> String
ppAST (Program {funDefList=fl}) = ( showString "Program" . nl . ppFunDefList 1 fl) "" 

ppFunDefList :: Int -> [FunDef] -> ShowS
ppFunDefList n fl = ppSeq n ppFunDef fl

ppFunDef :: Int -> FunDef -> ShowS
ppFunDef n (FunDef {funId=fId, funType=ft, bStmt=s, funSrcPos=_}) = 
    indent n . showString "FunDef" . nl .
    ppExpr (n+1) fId  .
	ppType (n+1) ft .
	ppStmt (n+1) s  
	
ppFieldId :: Int -> FieldId -> ShowS
ppFieldId n (FieldId {fieldIdName = fi,  fieldIdSrcPos = _ }) =
    indent n . showString "FieldId " . showString fi . nl


ppOp :: Int -> OpName -> ShowS
ppOp n o =
    indent n . showString "OP " . showString o . nl

ppFieldLit :: Int -> FieldLit -> ShowS
ppFieldLit n (FieldLit {fieldLitId=fi, fieldLitExpr=fe, fieldLitSrcPos=_}) = 
    indent n . showString "FieldLit" . nl .
    ppFieldId (n+1)  fi .
    ppExpr (n+1) fe 

ppType :: Int -> Type -> ShowS
ppType n (ArrayType {arrayType=at, typeSrcPos=_}) = 
    indent n . showString "ArrayType" . nl .
    ppType (n+1) at 

ppType n (RecordType {fieldTypeList=fl, typeSrcPos=_}) = 
    indent n . showString "RecordType" . nl .
    ppSeq (n+1) ppType fl

ppType n (FieldType {fieldId=fl, fieldType=ft, typeSrcPos=_}) =
    indent  n . showString "FieldType" . nl .
    ppFieldId (n+1) fl .
    ppType (n+1) ft 

ppType n (PrimitiveType {name = m, typeSrcPos=_}) =
    indent  n . showString "PrimitiveType " . showString m . nl

ppType n (FunType {recordType=rect, retType=rt, typeSrcPos=_}) = 
    indent n . showString "FunType" . nl .
    ppType (n+1) rect . 
    ppType (n+1) rt   

ppStmt :: Int -> Stmt -> ShowS
ppStmt n (VarDef {varId=v, varExpr=e, stmtSrcPos=_}) = 
   indent  n . showString "VarDef" . nl .
   ppExpr (n+1) v . 
   ppExpr  (n+1) e 

ppStmt n (AssignStmt {leftExpr=l, rightExpr=r, stmtSrcPos=_}) =
    indent  n . showString "AssignStmt" . nl .
    ppExpr (n+1) l .
    ppExpr (n+1) r

ppStmt n (BlockStmt {stmtList=sl, stmtSrcPos=_}) =
    indent  n . showString "BlockStmt" . nl .
    ppSeq (n+1) ppStmt sl 

ppStmt n (CallStmt {cExpr=c, stmtSrcPos=_}) =
    indent  n . showString "CallStmt" . nl .
    ppExpr (n+1) c 

ppStmt n (ForStmt {varId=v, forExpr=f, blockStmt=b, stmtSrcPos=_}) =
    indent  n . showString "ForStmt" . nl .
    ppExpr (n+1) v .
    ppExpr  (n+1) f .
    ppStmt (n+1) b

ppStmt n (IfStmt {bExpr =b, thenStmts=t, elseStmts=e, stmtSrcPos=_}) =
   indent  n . showString "IfStmt" . nl .
    ppExpr (n+1) b .
    ppStmt (n+1) t .
    maybe id (ppStmt (n+1)) e

ppStmt n (ReturnStmt {rExpr=r, stmtSrcPos=_}) =
   indent  n . showString "ReturnStmt" . nl .
   maybe id (ppExpr (n+1)) r
 
ppStmt n (WhileStmt {whileBoolExpr=w, whileStmts = ws, stmtSrcPos=_ }) =
    indent  n . showString "WhileStmt" . nl .
    ppExpr (n+1) w .
    ppStmt (n+1) ws

ppExpr :: Int -> Expr -> ShowS
ppExpr n (InfixExpr {op=o, inFixLeftExpr=l, inFixRightExpr=r, exprSrcPos=_ }) =
    indent  n . showString ("InfixExpr " ++ o) . nl .
    ppExpr (n+1) l .
    ppExpr (n+1) r 

ppExpr n (PrefixExpr {op=o, prefixExpr=pe, exprSrcPos=_}) =
    indent  n . showString ("PrefixExpr " ++ o ) .  nl .
    ppExpr (n+1) pe

ppExpr n (CallExpr {fNameExpr=f, argExprs=a, exprSrcPos=_}) =
    indent  n . showString "CallExpr" . nl .
    ppExpr (n+1) f .
    ppSeq (n+1) ppExpr a 

ppExpr n (CastExpr {castExpr=c, castType=ct, exprSrcPos=_}) =
    indent  n . showString "CastExpr" . nl .
    ppExpr (n+1) c .
    ppType (n+1) ct 

ppExpr n (FieldExpr {fieldExpr=f, fieldExprId=i, exprSrcPos=_}) =
    indent  n . showString "FieldExpr" . nl .
    ppExpr (n+1) f .
    ppFieldId (n+1) i 

ppExpr n (SubscriptExpr {sExpr=s, subscript=sub, exprSrcPos=_}) =
    indent  n . showString "SubscriptExpr" . nl .
    ppExpr (n+1) s .
    ppExpr (n+1) sub

ppExpr n (ParenExpr {pExpr=p, exprSrcPos=_}) =
    indent  n . showString "ParenExpr" . nl .
    ppExpr (n+1) p

ppExpr n (ArrayLit {arrayExprs=ae, exprSrcPos=_}) =
    indent  n . showString "ArrayLit" . nl .
    ppSeq (n+1) ppExpr ae

ppExpr n (RecordLit {recordFieldLits=rf, exprSrcPos=_}) =
    indent  n . showString "RecordLit" . nl .
    ppSeq (n+1) ppFieldLit rf 

ppExpr n (NullLit {exprSrcPos=_}) =
    indent  n . showString "NullLit" . nl 

ppExpr n (BoolLit {boolValue=True,exprSrcPos=_}) =
    indent  n . showString "BoolLit true" . nl 

ppExpr n (BoolLit {boolValue=False,exprSrcPos=_}) =
    indent  n . showString "BoolLit false" . nl 

ppExpr n (IntLit {intValue=i, exprSrcPos=_}) =
    indent  n . showString "IntLit " . showString (show i) . nl 

ppExpr n (StringLit {stringValue=s, exprSrcPos=_}) =
    indent  n . showString "StringLit " . ppName s . nl

ppExpr n (FunId { funIdName = fi, exprSrcPos = _ }) =
    indent n . showString "FunId " . showString fi . nl
	
ppExpr n (VarId { varIdName = vi, exprSrcPos = _ }) =
    indent n . showString "VarId " . showString vi . nl

indent :: Int -> ShowS
indent n = showString (take (2*n) (repeat ' '))

spc :: ShowS
spc = showChar ' '

nl :: ShowS
nl = showChar '\n'

ppSeq :: Int -> (Int -> a -> ShowS) -> [a] -> ShowS
ppSeq _ _ []      = id
ppSeq n pp (x:xs) = pp n x . ppSeq n pp xs

ppName :: Name -> ShowS
ppName n = showString (show n) 
