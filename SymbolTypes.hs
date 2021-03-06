module SymbolTypes where

import List

data TACK_TYPE  = TK_INT
                | TK_NULL
                | TK_STRING
                | TK_VOID
                | TK_BOOL
                | TK_RECORD [(String,TACK_TYPE)]
                | TK_ARRAY TACK_TYPE
                | TK_FUN String TACK_TYPE TACK_TYPE
                | TK_ERROR
                deriving (Eq)              

tkIntSize     = 8
tkStringSize  = 8
tkBoolSize    = 8
tkVoidSize    = 8
tkDefaultSize = 8

instance Show TACK_TYPE where
  show TK_INT = "int"
  show TK_NULL = "null"
  show TK_STRING = "string"
  show TK_VOID   = "void"
  show TK_BOOL   = "bool"
  show (TK_RECORD lT)  = "(" ++ tupleListToString lT ++ ")"
  show (TK_ARRAY t)    = "[ " ++ show t ++ " ]"
  show (TK_FUN _ a r)  = "" ++ show a ++ " -> " ++ show r ++ ""
  show TK_ERROR        = "Error"

getTypeSize :: TACK_TYPE -> Int
getTypeSize ty = 
  case ty of
    TK_INT    -> tkIntSize
    TK_STRING -> tkStringSize
    TK_BOOL   -> tkBoolSize
    TK_VOID   -> tkVoidSize
    TK_RECORD fieldList -> foldl (\s tp -> s + (getElemTypeSize $ snd tp) )  0 fieldList
    TK_ARRAY  elemT -> getElemTypeSize elemT
    TK_FUN  _ _ _ -> tkVoidSize
    _  -> tkDefaultSize 

getElemTypeSize :: TACK_TYPE -> Int
getElemTypeSize (TK_RECORD _) = tkVoidSize
getElemTypeSize (TK_ARRAY  _) = tkVoidSize
getElemTypeSize t           = getTypeSize t 
 
getFieldOffset :: TACK_TYPE -> String -> Int
getFieldOffset record fId =
  case record of
    TK_RECORD elemType -> getOffset 0 elemType
    _ -> error "record type is required"
  where getOffset os [] = error "no such field Id"
        getOffset os ((fI,ty):rest) = if fI == fId 
                                      then os
                                      else getOffset (os + (getElemTypeSize ty)) rest

tupleListToString :: [(String,TACK_TYPE)] -> String
tupleListToString ((x,y):[]) = x ++ ":" ++ show y 
tupleListToString ((x,y):xs) = x ++ ":" ++ show y ++ ", " ++ tupleListToString xs
tupleListToString []         = ""

listToString ::(Show a) => [a] -> String
listToString (x:[]) = show x
listToString (x:xs) = show x ++ ", "
listToString []     = ""


class TackType a where
  isBasicType :: a -> Bool
  isSame     :: a -> a -> Bool
  isSubType  :: a -> a -> Bool
  isCastable :: a -> a -> Bool
  isNULL     :: a -> Bool
  isRecord   :: a -> Bool
  isArray    :: a -> Bool
  isEmptyArray :: a -> Bool
  isEmptyRecord :: a -> Bool
 
instance TackType TACK_TYPE where
  isBasicType TK_INT    = True
  isBasicType TK_STRING = True
  isBasicType TK_BOOL   = True
  isBasicType TK_NULL   = True
  isBasicType _         = False

  isSame a b = a == b

  -- isSubType = a >= b
  isSubType TK_VOID       b      = True
  isSubType TK_NULL       b      = True
  isSubType (TK_RECORD a) (TK_RECORD b) = 
    a == b || ( ( length a  <= length b ) 
    && (maybe True (\x -> False) $ find (==False) (zipWith (\x y-> (fst x == fst y) && isSubType (snd x) (snd y)) a b))) 
  isSubType (TK_ARRAY a)  (TK_ARRAY b)  = isEmptyRecord(a) || isSame a b 
  isSubType a             b             = isSame a b
 
  -- isCastable = a <-> b
  isCastable a b =
    case (isBasicType a, isBasicType b) of
      (True, True) -> True 
      _ -> isSubType a b || isSubType b a

  isNULL   a = a == TK_NULL
  isRecord a = 
    case a of 
      TK_RECORD _ -> True
      otherwise  -> False

  isArray a = 
    case a of 
      TK_ARRAY  _ -> True
      otherwise  -> False

  isEmptyArray (TK_ARRAY TK_NULL) = True
  isEmptyArray (TK_ARRAY t)       = isEmptyArray t
  isEmptyArray _                  = False

  isEmptyRecord (TK_RECORD []) = True
  isEmptyRecord _ = False
  

-- 
isSubTypeList :: (TackType a) => [a] -> [a] -> Bool 
isSubTypeList (x:xs) (y:ys) = (isSubType x y) && (isSubTypeList xs ys)
isSubTypeList  []    []     = True
isSubTypeList  x     []     = False
isSubTypeList  []    y      = True


-- types for symbol table
data SYMBOL_VALUE = VAR_ATTR (Int, TACK_TYPE)
            | FUN_ATTR (String, TACK_TYPE, TACK_TYPE)
            | SUBST_ATTR (Int, SYMBOL, TACK_TYPE)
            deriving (Show, Eq)

data SYMBOL_DESC = ARGUMENT(SYMBOL, TACK_TYPE)
                 | VARIABLE(SYMBOL, TACK_TYPE)
                 | FUNCTION(SYMBOL, TACK_TYPE, TACK_TYPE)
                 | SUBST(SYMBOL, SYMBOL, TACK_TYPE)
 
data SYMBOL_I_DESC = I_VARIABLE(Int, Int, TACK_TYPE)
                   | I_FUNCTION(Int, String, TACK_TYPE, TACK_TYPE)
                   | I_SUBST(Int, Int, SYMBOL, TACK_TYPE)
                   | NO_FOUND
                   deriving (Eq)

type SYMBOL = String

getTackTypeFromSymbolDesc :: SYMBOL_I_DESC -> TACK_TYPE
getTackTypeFromSymbolDesc (I_VARIABLE(_, _, t)) = t
getTackTypeFromSymbolDesc (I_FUNCTION(_, fId, aT, rT)) = TK_FUN fId aT rT
getTackTypeFromSymbolDesc (I_SUBST(_, _,_, vT)) =  vT
getTackTypeFromSymbolDesc NO_FOUND = TK_ERROR


