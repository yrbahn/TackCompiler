module SrcPos where

data SrcPos 
    = NoSrcPos
    | SrcPos {
        spLine :: Int,
    	spCol  :: Int
    }
    deriving (Eq, Ord)

instance Show SrcPos where
    showsPrec _ NoSrcPos = showString "unknown position"
    showsPrec _ (SrcPos {spLine=l, spCol= c}) =
        showString "line "
       	. shows l
	. showString ", column "
	. shows c

class HasSrcPos a where
    srcPos :: a -> SrcPos

instance HasSrcPos a => HasSrcPos [a] where
    srcPos [] = NoSrcPos
    srcPos (x:_) = srcPos x
    
