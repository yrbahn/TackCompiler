module ASM where
import Control.Monad.Trans
import SymbolTable

data AsmProg = AsmProg Directive [Function]

instance Show AsmProg where
  show (AsmProg d funList) = 
    show d ++ "\n" ++ (unlines $ map (\x-> show x) funList)

data Directive =
  IntelSyntax
  | Type String String
  | Global String
  | Def String
  | Size String String

instance Show Directive where
  show IntelSyntax = ".intel_syntax"
  show (Type a b)  = ".type " ++ a ++ ", " ++ b
  show (Global a)  = ".global " ++ a 
  show (Size a b)  = ".size " ++ a ++ ", " ++ b
  show (Def d)  = d ++ ":"

data Function = 
  Function Section (Maybe Section)

instance Show Function where
  show (Function s maybeS) = 
    case maybeS of 
      Nothing -> show s
      Just s' -> show s ++ show s'

data Section = 
  TextSection [Directive] [Instruction]
  | DataSection [Directive]

instance Show Section where
  show (TextSection directiveList instructList) = 
    ".text\n" ++ (unlines $ map (\x-> show x) directiveList) 
    ++ (unlines $ map (\x -> show x) instructList) 
  show (DataSection dList) = 
    ".section .rodata\n" ++ (unlines $ map (\x -> show x) dList)  
    
type Label = String

data Instruction = 
  Define String
  | Add Operand Operand
  | Call Label
  | Cmp Operand Operand
  | IDiv Operand
  | IMul Operand Operand
  | Jmp Label
  | Je Label
  | Jg Label
  | Jge Label
  | Jl Label
  | Jle Label
  | Jne Label
  | Mov Operand Operand
  | Neg Operand
  | Pop Operand
  | Push Operand
  | Ret
  | Sal Operand Operand
  | Sar Operand Operand
  | Shr Operand Operand
  | Sub Operand Operand

instance Show Instruction where
  show (Define d) = d ++ ":"
  show (Add a b)  = "add " ++ show a ++ ", " ++ show b
  show (Call n)   = "call " ++ n
  show (Cmp a b)  = "cmp " ++ show a ++ ", " ++ show b
  show (IDiv a)   = "idiv " ++ show a
  show (IMul a b) = "imul " ++ show a ++ ", " ++ show b
  show (Jmp l)    = "jmp " ++ l
  show (Je l)     = "je " ++ l
  show (Jge l)    = "jge " ++ l
  show (Jl l)     = "jl " ++ l
  show (Jle l)    = "jle " ++ l
  show (Jne l)    = "jne " ++ l
  show (Mov a b)  = "mov " ++ show a ++ ", " ++ show b
  show (Neg a)    = "neg " ++ show a
  show (Pop a)    = "pop " ++ show a
  show (Push a)   = "push " ++ show a
  show Ret        = "ret"
  show (Sal a b)  = "sal " ++ show a ++ ", " ++ show b
  show (Sar a b)  = "sar " ++ show a ++ ", " ++ show b
  show (Shr a b)  = "shr " ++ show a ++ ", " ++ show b
  show (Sub a b)  = "sub " ++ show a ++ ", " ++ show b


data Operand = 
  Res Register
  | ImmNum Int
  | LabelOperand String
  | Mem String
  deriving (Eq)

instance Show Operand where
  show (Res r) = show r
  show (ImmNum i)     = show i
  show (LabelOperand s)  = s
  show (Mem s)        = s

data Register = RAX | RBX | RCX | RDX | RSP | RBP | RSI | RDI 
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq)

instance Show Register where
  show RAX = "%rax"
  show RBX = "%rbx"
  show RCX = "%rcx"
  show RDX = "%rdx"
  show RSP = "%rsp"
  show RBP = "%rbp"
  show RSI = "%rsi"
  show RDI = "%rdi"
  show R8  = "%r8"
  show R9  = "%r9"
  show R10 = "%r10"
  show R11 = "%r11"
  show R12 = "%r12"
  show R13 = "%r13"
  show R14 = "%r14"
  show R15 = "%r15"

data ASMState = ASMState
  { st::ST, symbols :: [(String,Int)] }

emptyASMState = ASMState { st = empty, symbols = []}

newtype ASMTranslator m a = ASMTranslator 
  { runASM :: ASMState -> m (a, ASMState) }

instance Monad m => Monad (ASMTranslator m) where
  return a = ASMTranslator (\s -> return (a, s))
  m >>= f  = ASMTranslator (\s -> 
             do 
               (a1, s') <- runASM m s
               (a2, s'') <- runASM (f a1) s'
               return (a2, s''))

instance MonadTrans ASMTranslator where
  lift m = ASMTranslator $ \ns -> do a <- m
                                     return (a, ns)

putASMState :: Monad m => a -> ASMTranslator m ()
putASMState s = ASMTranslator $ \s -> return ((), s) 

getASMState :: Monad m => ASMTranslator m ASMState
getASMState = ASMTranslator $ \s -> return (s, s)
