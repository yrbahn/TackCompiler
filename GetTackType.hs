module GetTackType where
import AST
import SymbolTypes
import SymbolTable
import List

getTackType :: Type -> IO TACK_TYPE
getTackType ArrayType{arrayType=aT, typeSrcPos=_} =
  do
   elementType <- getTackType aT
   return $ TK_ARRAY elementType

getTackType RecordType{fieldTypeList=fL, typeSrcPos=_} =
  do
    fLType <- mapM getFieldType fL
    return $ TK_RECORD fLType 
    where 
      getFieldType FieldType{fieldId=FieldId{fieldIdName=fId, fieldIdSrcPos=_}, fieldType=fT,typeSrcPos=_} =
        do
         fType <- getTackType fT
         return $ (fId,fType)

getTackType FunType{recordType=rT, retType=retT, typeSrcPos=_} =
  do
    argType <- getTackType rT
    retType <- getTackType retT
    return $ TK_FUN "" argType retType
 
getTackType PrimitiveType{name=t, typeSrcPos=_} =
  case t of
    "bool"   -> return TK_BOOL
    "int"    -> return TK_INT
    "string" -> return TK_STRING
    "void"   -> return TK_VOID
    _        -> return TK_ERROR 

getTackTypeFromExpr :: Int -> ST -> Expr -> IO TACK_TYPE
getTackTypeFromExpr level st (InfixExpr{op=o, inFixLeftExpr=lE, inFixRightExpr=rE, exprSrcPos=_}) =
  getTackTypeFromExpr level st lE

getTackTypeFromExpr level st (PrefixExpr{op=o,prefixExpr=pE,exprSrcPos=sP}) = 
  getTackTypeFromExpr level st pE

getTackTypeFromExpr level st (CallExpr {fNameExpr=fN,argExprs=aE,exprSrcPos=_}) =
  do
    funType <- getTackTypeFromExpr level st fN
    case funType of
      TK_FUN _ _ rType -> return rType
      _ -> return TK_NULL
     
getTackTypeFromExpr level st (CastExpr {castExpr=cE,castType=cT,exprSrcPos=_})  =
  getTackType cT

getTackTypeFromExpr level st (SubscriptExpr{sExpr=sE, subscript=subE,exprSrcPos=sP}) =
  do
    baseType <- getTackTypeFromExpr level st sE
    case baseType of
      TK_ARRAY t -> return t
      _          -> return TK_NULL

getTackTypeFromExpr level st (FieldExpr{fieldExpr=fE, fieldExprId=FieldId{fieldIdName=fId,fieldIdSrcPos=fSP},exprSrcPos=_}) =
  do
    elemListType <- getTackTypeFromExpr level st fE
    case elemListType of
      TK_RECORD fieldL ->
        case (find (\x-> (fst x) ==  fId) fieldL) of
          Just t -> return $ snd t
          Nothing -> return TK_NULL
      _ -> return $ TK_RECORD []

getTackTypeFromExpr level st (ParenExpr{pExpr=pE,exprSrcPos=sP})  =
  getTackTypeFromExpr level st pE

getTackTypeFromExpr level st (ArrayLit {arrayExprs=aEL,exprSrcPos=sP}) =
  if length aEL == 0
    then return $ TK_ARRAY TK_NULL
    else do 
           elemType <- getTackTypeFromExpr level st (head aEL)
           return $ TK_ARRAY elemType
 
getTackTypeFromExpr level st (RecordLit {recordFieldLits=rFL,exprSrcPos=sP}) =
  do
    tupleListType <-  mapM (getTackTypeFromFieldLit level st) rFL
    return $ TK_RECORD tupleListType
 
getTackTypeFromExpr level st (BoolLit{boolValue=bV,exprSrcPos=_}) =
  return TK_BOOL

getTackTypeFromExpr level st (IntLit{intValue=iV,exprSrcPos=_}) =
  return TK_INT

getTackTypeFromExpr level st (NullLit{exprSrcPos=_}) =
  return TK_NULL

getTackTypeFromExpr level st (StringLit{stringValue=sV,exprSrcPos=_})  =
  return TK_STRING

getTackTypeFromExpr level st (FunId{funIdName=fId,exprSrcPos=sP}) =
  case look_up st fId of
    I_FUNCTION(_, _, argType, retType) -> return $ TK_FUN fId argType retType
    I_VARIABLE(_,_,varType) -> 
        return $ TK_FUN fId (TK_RECORD []) TK_NULL
    _ -> 
        return $ TK_NULL

getTackTypeFromExpr level st (VarId{varIdName=vId,exprSrcPos=sP}) =
  case look_up st vId of
    I_VARIABLE(_,_, tackType) -> return tackType
    I_FUNCTION(_,_,argT,retT) -> 
      return $ retT
    _ -> 
      return TK_NULL
       

getTackTypeFromFieldLit :: Int -> ST -> FieldLit -> IO (String, TACK_TYPE)
getTackTypeFromFieldLit level st  (FieldLit{fieldLitId=fId, fieldLitExpr=fE, fieldLitSrcPos=sP}) =
  do
    fieldType <- getTackTypeFromExpr level st fE
    return (fieldIdName fId, fieldType)

 
