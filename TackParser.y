{
module TackParser where
import Prelude hiding (GT, LT, EQ)
import Char
import TackLexer
import AST
import PrettyPrint
import SrcPos
import Token

}

%name parseTack
%tokentype { Token }
%monad { Alex }
%lexer { tackLexer } { EOFToken }


%token
    IDENT       { Token (ID _) _ } 
    INT_LIT     { Token (INT _) _ }
    STRING_LIT  { Token (STRING _) _ }
   'else'       { Token ELSE _ }
   'false'      { Token FALSE _  }
   'for'        { Token FOR _ }
   'fun'        { Token FUNCTION _ }
   'if'         { Token IF _ }
   'in'         { Token IN _ }
   'null'       { Token NULL _ }
   'true'       { Token TRUE _ }
   'void'       { Token TVOID _ }
   'while'      { Token WHILE _ }
   'bool'       { Token TBOOL _ }
   'int'        { Token TINT _ }
   'string'     { Token TSTRING _ }
   '('          { Token LPAREN _ }
   ')'          { Token RPAREN _ }
   '['          { Token LBRACK _ }
   ']'          { Token RBRACK _ }
   '{'          { Token LBRACE _ }
   '}'          { Token RBRACE _ }
   ':'          { Token COLON _  }
   '->'         { Token ARROW _ }
   '='          { Token FEQ _ }
   ':='         { Token ASSIGN _ }
   ','          { Token COMMA _}
   ';'          { Token SEMICOLON _ }
   '.'          { Token DOT _ }
   '!'          { Token NOT _ }
   '*'          { Token TIMES _ }
   '/'          { Token DIVIDE _ }
   '%'          { Token MODE _ }
   '+'          { Token PLUS _ }
   '-'          { Token MINUS _ }
   '<='         { Token LE _ }
   '<'          { Token LT _ }
   '>='         { Token GE _ }
   '>'          { Token GT _ }
   '=='         { Token EQ _ }
   '!='         { Token NEQ _ }
   '&&'         { Token AND _ }
   '||'         { Token OR _ }


%left '||'
%left '&&'
%nonassoc '==' '!=' 
%nonassoc '>' '>=' '<' '<='
%left  '+' '-'
%left  '*' '/' '%'
%%

program :: { AST }
program : funDefList            { Program $1 }

funDefList :: { [FunDef] }
funDefList : funDef    { [$1]  }   
           | funDef funDefList { $1:$2 }


funDef  :: { FunDef } 
funDef  
    : IDENT '=' 'fun' funType blockStmt 
        { FunDef { funId = FunId (getString $1) (getPos $1) , funType = $4, bStmt = $5, funSrcPos = getPos $1 } }

type  :: { Type }
type
    : arrayType    { $1 }
    | recordType   { $1 }
    | 'bool'       { PrimitiveType { name = "bool"  , typeSrcPos = getPos $1  } }
    | 'int'        { PrimitiveType { name = "int"   , typeSrcPos = getPos $1  } }
    | 'string'     { PrimitiveType { name = "string", typeSrcPos = getPos $1  } }

arrayType :: { Type }
arrayType
    : '[' type ']' { ArrayType { arrayType = $2, typeSrcPos = getPos $1 }}

recordType :: { Type }
recordType
    : '(' fieldTypeList ')' { RecordType {fieldTypeList = $2, typeSrcPos = getPos $1 } }
 
fieldType :: { Type }
fieldType
    : fieldId ':' type  { FieldType { fieldId = $1, fieldType = $3, typeSrcPos = getPos $2 }}
fieldTypeList :: { [Type] }
fieldTypeList
    :      { [] }
    | fieldType fieldTypeTail  
        { $1:$2 }

fieldTypeTail :: { [Type] }
fieldTypeTail
    :                          { [] }
    |','  fieldType fieldTypeTail  
        {  $2:$3  }

funType :: { Type }
funType
    : recordType '->' returnType { FunType{ recordType = $1, retType = $3, typeSrcPos = getPos $2 } }

returnType  :: { Type }
returnType
    : type   { $1 }
    | 'void' { PrimitiveType { name = "void", typeSrcPos = getPos $1  } }
 
stmt :: {Stmt} 
stmt 
    : varDef      { $1 }
    | assignStmt  { $1 }
    | blockStmt   { $1 }
    | callStmt    { $1 }
    | forStmt     { $1 }
    | ifStmt      { $1 }
    | returnStmt  { $1 }
    | whileStmt   { $1 }

stmts :: { [Stmt] }
stmts
    :              { [] }    
    | stmt stmts   { $1:$2 }

varDef :: { Stmt }
varDef 
    : IDENT '=' expr ';'  
        { VarDef { varId = VarId (getString $1) (getPos $1), varExpr = $3, stmtSrcPos = getPos $2 } }

assignStmt :: { Stmt } 
assignStmt 
    : expr ':=' expr  ';'  { AssignStmt { leftExpr = $1, rightExpr = $3, stmtSrcPos = getPos $2 } }
 
blockStmt :: { Stmt }
blockStmt 
    : '{' stmts '}'  { BlockStmt { stmtList = $2 , stmtSrcPos = getPos $1 } }
 
callStmt :: { Stmt }
callStmt 
    : callExpr ';'  { CallStmt { cExpr = $1, stmtSrcPos = getPos $2 } }

forStmt :: { Stmt }
forStmt 
    : 'for' IDENT 'in' expr blockStmt  
        { ForStmt { varId = VarId (getString $2) (getPos $2), forExpr = $4, blockStmt = $5, stmtSrcPos = getPos $1 } }

ifStmt :: { Stmt }
ifStmt 
    : 'if' expr blockStmt ifElseStmt  {IfStmt { bExpr = $2, thenStmts = $3, elseStmts = $4, stmtSrcPos = getPos $1 } }

ifElseStmt :: { Maybe Stmt }
ifElseStmt 
    :                   { Nothing }
    | 'else' blockStmt  { Just $2 }
 
returnStmt :: { Stmt }
returnStmt 
    : '->' ';'       { ReturnStmt { rExpr = Nothing, stmtSrcPos = getPos $1 } }
    | '->' expr ';'  { ReturnStmt { rExpr = Just $2, stmtSrcPos = getPos $1 } }

whileStmt :: { Stmt } 
whileStmt 
    : 'while' expr blockStmt { WhileStmt { whileBoolExpr = $2, whileStmts = $3, stmtSrcPos = getPos $1 } }

expr :: { Expr }
expr 
    : logicOrExpr   { $1 VarId {varIdName="", exprSrcPos=NoSrcPos} }

logicOrExpr :: { Expr -> Expr }
logicOrExpr 
    : logicAndExpr logicOrExprTail { \inher -> $2 ($1 inher) }

logicOrExprTail :: { Expr -> Expr }
logicOrExprTail 
    : logicOrOp  logicAndExpr logicOrExprTail   
        { \inher -> $3 InfixExpr {op=getString $1 , inFixLeftExpr=inher, inFixRightExpr= ($2 inher), exprSrcPos = getPos $1} }
    |   { \inher -> inher }

logicAndExpr :: { Expr -> Expr }
logicAndExpr 
    : eqExpr logicAndExprTail  {\inher -> $2 ($1 inher) }

logicAndExprTail :: { Expr -> Expr }
logicAndExprTail 
    : logicAndOp eqExpr logicAndExprTail   
        { \inher -> $3 InfixExpr {op="&&", inFixLeftExpr=inher, inFixRightExpr=($2 inher), exprSrcPos = getPos $1} }        
    |   { \inher -> inher }

eqExpr :: { Expr -> Expr }
eqExpr 
    :  relExpr eqExprTail  {\inher -> $2 ($1 inher) }

eqExprTail :: { Expr -> Expr }
eqExprTail 
    : eqOp relExpr eqExprTail 
        { \inher -> $3 InfixExpr {op=getString $1, inFixLeftExpr=inher, inFixRightExpr=($2 inher), exprSrcPos = getPos $1} }        
    |   { \inher -> inher }

relExpr :: { Expr -> Expr }
relExpr 
    : addExpr relExprTail  {\inher -> $2 ($1 inher) }  

relExprTail :: { Expr -> Expr } 
relExprTail
    : relOp addExpr relExprTail 
        { \inher -> $3 InfixExpr {op=getString $1, inFixLeftExpr=inher, inFixRightExpr=($2 inher), exprSrcPos = getPos $1} }        
    |   { \inher -> inher }

addExpr :: { Expr -> Expr }
addExpr 
    : multExpr addExprTail  { \inher -> $2 ($1 inher) }

addExprTail :: { Expr -> Expr }
addExprTail
    : addOp multExpr addExprTail 
        { \inher -> $3 InfixExpr {op=getString $1, inFixLeftExpr=inher, inFixRightExpr= ($2 inher), exprSrcPos = getPos $1} }        
    |   { \inher -> inher }

multExpr :: { Expr -> Expr }
multExpr 
    : prefixExpr multExprTail  {\inher -> $2 ($1 inher) }

multExprTail :: { Expr -> Expr }
multExprTail 
    : mulOp prefixExpr multExprTail 
        { \inher -> $3 InfixExpr {op=getString $1, inFixLeftExpr=inher, inFixRightExpr=($2 inher), exprSrcPos = getPos $1} }        
    |   { \inher -> inher }
  
prefixExpr :: { Expr -> Expr }
prefixExpr 
    : prefixOp prefixExpr  
         {\inher -> PrefixExpr { op = getString $1, prefixExpr = $2 inher, exprSrcPos = getPos $1 } }
    | postfixExpr   {\inher -> $1 inher }

postfixExpr :: { Expr -> Expr }
postfixExpr 
    : primExpr postfixExprTail {\inher -> $2 $1}

postfixExprTail :: { Expr -> Expr }
postfixExprTail
    : callExprTail      { \inher -> $1 inher}
    | castExprTail      { \inher -> $1 inher}
    | fieldExprTail     { \inher -> $1 inher}
    | subscriptExprTail { \inher -> $1 inher}
    | emptyPostfixTail  { \inher -> inher }

emptyPostfixTail :: { } 
emptyPostfixTail
    :               {}

primExpr :: { Expr }
primExpr
    : varId      { $1 }
    | arrayLit   { $1 }
    | recordLit  { $1 }
    | parenExpr  { $1 }
    | boolLit    { $1 }
    | intLit     { $1 }
    | nullLit    { $1 }
    | stringLit  { $1 }

varId :: { Expr }
varId 
    : IDENT { VarId { varIdName = getString $1, exprSrcPos = getPos $1 } }

funId :: { Expr }
funId 
    : IDENT { FunId { funIdName = getString $1, exprSrcPos = getPos $1 } }


fieldId :: { FieldId }
fieldId 
    : IDENT { FieldId { fieldIdName = getString $1, fieldIdSrcPos = getPos $1 } }


arrayLit :: { Expr }
arrayLit
    : '[' exprs ']' { ArrayLit { arrayExprs = $2, exprSrcPos = getPos $1 } }

recordLit :: { Expr }
    : '(' fieldLits ')'  { RecordLit { recordFieldLits = $2, exprSrcPos = getPos $1 } }

parenExpr :: { Expr }
parenExpr  
 : '(' expr ')'  { ParenExpr { pExpr = $2, exprSrcPos = getPos $1 } }
 
boolLit :: { Expr }
boolLit 
    : 'false' { BoolLit { boolValue = False , exprSrcPos = getPos $1 } }
    | 'true'  { BoolLit { boolValue = True , exprSrcPos = getPos $1 } }

intLit :: { Expr }
intLit 
    : INT_LIT    { IntLit  { intValue  = getInt $1,  exprSrcPos = getPos $1 } }

nullLit :: { Expr }
nullLit 
    : 'null'     { NullLit { exprSrcPos = getPos $1 } }

stringLit :: { Expr }
stringLit 
    : STRING_LIT  { StringLit { stringValue = getString $1, exprSrcPos = getPos $1 } }

exprs :: { [Expr] } 
exprs 
    :                 { []    }
    | expr exprsTail  { $1:$2 }

exprsTail :: { [Expr] }
exprsTail
    :                     { []    }
    | ',' expr exprsTail  { $2:$3 } 

callExpr :: { Expr }
callExpr 
    : funId callActuals { CallExpr { fNameExpr = $1, argExprs = $2, exprSrcPos = srcPos $1 } }

callActuals :: { [Expr] }
callActuals 
    : '(' exprs ')' emptyPostfixTail { $2 };

callExprTail :: { Expr -> Expr }
callExprTail 
    : '(' exprs ')' postfixExprTail { \inher -> $4  CallExpr { fNameExpr = changeFunId inher, argExprs = $2, exprSrcPos = getPos $1 } }

castExprTail :: { Expr -> Expr }
castExprTail 
    :  ':' type postfixExprTail { \inher -> $3 CastExpr { castExpr = inher , castType = $2, exprSrcPos = getPos $1 } }

fieldExprTail :: { Expr -> Expr }
fieldExprTail 
    :  '.' fieldId postfixExprTail
        { \inher -> $3 FieldExpr { fieldExpr = inher, fieldExprId = $2, exprSrcPos = getPos $1 } }

subscriptExprTail :: { Expr -> Expr }
subscriptExprTail
    :  '[' expr ']'  postfixExprTail { \inher -> $4 SubscriptExpr { sExpr = inher, subscript = $2, exprSrcPos = getPos $1 } }


fieldLit :: { FieldLit }
fieldLit 
    : IDENT '=' expr  
        { FieldLit { fieldLitId = FieldId (getString $1) (getPos $1), fieldLitExpr = $3, fieldLitSrcPos = getPos $1 } }
 
fieldLits :: { [FieldLit] }   
fieldLits 
    :                         { []    }
    | fieldLit fieldLitsTail  { $1:$2 }

fieldLitsTail :: { [FieldLit] }   
fieldLitsTail 
    :                               { []    }
    | ',' fieldLit fieldLitsTail    { $2:$3 }

logicOrOp :: { Token }
logicOrOp 
    : '||'  { $1 }

logicAndOp :: { Token }
logicAndOp 
    : '&&'  { $1 }

eqOp :: { Token }
eqOp
    : '=='  { $1 }
    | '!='  { $1 }

relOp :: { Token }
relop 
    : '<=' { $1 }
    | '<'  { $1 }
    | '>=' { $1 }
    | '>'  { $1 }

addOp :: { Token }
addOp
    : '+'  { $1 }
    | '-'  { $1 }

mulOp :: { Token }
mulOp 
    : '*'  { $1 }
    | '/'  { $1 }
    | '%'  { $1 }

prefixOp :: { Token }
prefixOp 
    : '-'   { $1 }
    | '!'   { $1 }

{

happyError :: Alex a
happyError = do
        line <- alexGetLine
        column <- alexGetColumn
        alexError ("line " ++ show line ++ ", column " ++ show column ++": parser error")

parse :: String -> Either String AST
parse str = runAlex str parseTack

getString :: Token -> String
getString (Token (ID name) _)   = name
getString (Token (STRING s) _)  = s
getString (Token PLUS _)   = "+"
getString (Token TIMES _)   = "*" 
getString (Token DIVIDE _) = "/"
getString (Token MODE _)   = "%" 
getString (Token MINUS _)  = "-" 
getString (Token LE _) = "<=" 
getString (Token LT _) = "<" 
getString (Token GE _) = ">=" 
getString (Token GT _) = ">" 
getString (Token EQ _) = "==" 
getString (Token NEQ _) = "!=" 
getString (Token AND _) = "&&" 
getString (Token OR _) = "||" 
getString (Token NOT _) = "!"
getString _  ="error"

getInt ::Token -> Int
getInt (Token (INT i) _) = i
getInt _ = 0

getBool :: Token -> Bool
getBool (Token FALSE _) = False
getBool (Token TRUE _)  = True
getBool _               = False


changeFunId :: Expr -> Expr
changeFunId (VarId {varIdName=v, exprSrcPos=e} ) = FunId v e 
changeFunId x = x

{-
main = do
    inStr <- getContents
    case parse inStr of
        Left msg ->  putStrLn msg
        Right ast -> putStrLn $ ppAST ast
-}
}
