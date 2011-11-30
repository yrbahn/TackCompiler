module AST where
import SrcPos

type OpName  = String
type Name    = String
		
data AST = 
    Program { 
        funDefList :: [FunDef] }

data FieldId = 
    FieldId {
        fieldIdName :: String,
        fieldIdSrcPos :: SrcPos }
        
data FieldLit =  
    FieldLit { 
        fieldLitId  :: FieldId,
        fieldLitExpr :: Expr,
        fieldLitSrcPos :: SrcPos }

instance HasSrcPos AST where
    srcPos  = srcPos . funDefList
   
instance HasSrcPos FieldLit where
    srcPos = fieldLitSrcPos 
	
data FunDef = 
    FunDef { 
        funId :: Expr,
        funType :: Type,
        bStmt  :: Stmt,
        funSrcPos :: SrcPos }


instance HasSrcPos FunDef where
    srcPos = funSrcPos

-- Type 

data Type
    = ArrayType { 
        arrayType :: Type,
        typeSrcPos :: SrcPos }
    | RecordType { 
        fieldTypeList :: [Type],
        typeSrcPos :: SrcPos }
    | FieldType { 
        fieldId :: FieldId,
        fieldType :: Type,
        typeSrcPos :: SrcPos }
    | PrimitiveType { 
        name :: String,
        typeSrcPos :: SrcPos }
    | FunType { 
        recordType :: Type,
        retType :: Type,
        typeSrcPos :: SrcPos }

instance HasSrcPos Type where
    srcPos = typeSrcPos

data Stmt 
    = VarDef { 
        varId :: Expr,
        varExpr :: Expr,
        stmtSrcPos :: SrcPos }
    | AssignStmt { 
        leftExpr :: Expr,
        rightExpr :: Expr,
        stmtSrcPos :: SrcPos }
    | BlockStmt { 
        stmtList :: [Stmt],
	    stmtSrcPos :: SrcPos }
    | CallStmt { 
        cExpr :: Expr ,
        stmtSrcPos :: SrcPos }
    | ForStmt { 
        varId :: Expr,
        forExpr :: Expr,
        blockStmt :: Stmt,
        stmtSrcPos :: SrcPos }
    | IfStmt { 
        bExpr :: Expr,
        thenStmts :: Stmt,
        elseStmts :: Maybe Stmt,
        stmtSrcPos :: SrcPos }
    | ReturnStmt { 
        rExpr :: Maybe Expr, 
        stmtSrcPos :: SrcPos }
    | WhileStmt { whileBoolExpr :: Expr,
        whileStmts :: Stmt,
        stmtSrcPos :: SrcPos }

instance HasSrcPos Stmt where
    srcPos = stmtSrcPos

data Expr 
    = InfixExpr { 
        op :: OpName,
        inFixLeftExpr :: Expr,
        inFixRightExpr :: Expr,
        exprSrcPos :: SrcPos }
    | PrefixExpr { 
        op :: OpName,
        prefixExpr :: Expr,
        exprSrcPos :: SrcPos }
    | CallExpr { 
        fNameExpr :: Expr,
        argExprs  :: [Expr],
        exprSrcPos :: SrcPos }
    | CastExpr { 
        castExpr :: Expr,
        castType :: Type,
        exprSrcPos :: SrcPos }
    | FieldExpr	 { 
        fieldExpr :: Expr,
        fieldExprId :: FieldId,
        exprSrcPos :: SrcPos }
    | SubscriptExpr { 
        sExpr :: Expr ,
        subscript :: Expr,
        exprSrcPos :: SrcPos }
    | ParenExpr { 
        pExpr :: Expr,
        exprSrcPos :: SrcPos }
    | ArrayLit { 
        arrayExprs :: [Expr],
        exprSrcPos :: SrcPos }
    | RecordLit { 
        recordFieldLits :: [FieldLit],
        exprSrcPos :: SrcPos }
    | BoolLit  { 
        boolValue :: Bool,
        exprSrcPos :: SrcPos }
    | IntLit { 
        intValue :: Int,
      	exprSrcPos :: SrcPos }
    | NullLit { 
        exprSrcPos :: SrcPos }
    | StringLit { 
        stringValue :: String,
        exprSrcPos :: SrcPos }
    | FunId {
	funIdName :: String,
        exprSrcPos :: SrcPos }
    | VarId {
	varIdName :: String,
        exprSrcPos :: SrcPos }

instance  HasSrcPos Expr where
    srcPos = exprSrcPos


isRelOp :: String -> Bool
isRelOp "==" = True
isRelOp "!=" = True
isRelOp ">"  = True
isRelOp ">=" = True
isRelOp "<"  = True
isRelOp "<=" = True
isRelOp _    = False

isInfixOp :: String -> Bool
isInfixOp "*" = True
isInfixOp "-" = True
isInfixOp "/" = True
isInfixOp "%" = True
isInfixOp "+" = True
isInfixOp _   = False

isPrefixOp :: String -> Bool
isPrefixOp "-" = True
isPrefixOp "!" = True
isPrefixOp _   = False

