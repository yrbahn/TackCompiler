module GlobalVar where
import Data.IORef
import System.IO.Unsafe
import IR

aTag = unsafePerformIO $ newIORef 0 
lTag = unsafePerformIO $ newIORef 0 

genNewAddr :: IO IAddr
genNewAddr =
  do 
    t <- readIORef aTag
    writeIORef aTag $  t + 1
    return $ IID $ "t" ++ show t

genNewLabel :: IO ILabel
genNewLabel =
  do
    l <- readIORef lTag
    writeIORef lTag $ l + 1
    return $ "label" ++ show l 

