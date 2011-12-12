module GlobalVar where
import Data.IORef
import System.IO.Unsafe
import IR
import SymbolTypes

aTag = unsafePerformIO $ newIORef 0 
lTag = unsafePerformIO $ newIORef 0 

genNewAddr :: TACK_TYPE -> IO IAddr
genNewAddr ty=
  do 
    t <- readIORef aTag
    writeIORef aTag $  t + 1
    return $ IID ( "t" ++ show t) ty

genNewLabel :: IO ILabel
genNewLabel =
  do
    l <- readIORef lTag
    writeIORef lTag $ l + 1
    return $ "label" ++ show l 

