{
{-# OPTIONS -w -funbox-strict-fields #-}

module TackLexer where 
import Prelude hiding (GT, LT, EQ)
import SrcPos
import Token
import Data.Maybe
import Data.Char ( chr, readLitChar )
import Data.Map (Map)
import qualified Data.Map as M(empty)
import Numeric ( readDec )
}

%wrapper "monadUserState"

$whitespace = [\ \t\n\f\v\r]
$digit      = [0-9]
$letter     = [a-zA-Z]

@int        = ([1-9]$digit* | 0)
@identifier = ($letter | \_)[$letter \_ $digit]*
@comment    = "#".*

tokens :-

<0>				"fun"			{ pack (\s -> FUNCTION) }
<0>				"in"			{ pack (\s -> IN) }
<0>				"null"			{ pack (\s -> NULL) }
<0>				"for"			{ pack (\s -> FOR) }
<0>				"while"			{ pack (\s -> WHILE) }
<0>				"else"			{ pack (\s -> ELSE) }
<0>				"if"			{ pack (\s -> IF) }
<0>				"void"			{ pack (\s -> TVOID) }
<0>				"bool"			{ pack (\s -> TBOOL) }
<0>				"string"		{ pack (\s -> TSTRING) }
<0>				"int"			{ pack (\s -> TINT) }
<0>				"false"			{ pack (\s -> FALSE) }
<0>				"true"			{ pack (\s -> TRUE) }
<0>				\-\>			{ pack (\s -> ARROW) }
<0>				:\=			{ pack (\s -> ASSIGN) }
<0>				\|\|			{ pack (\s -> OR) }
<0>				&&			{ pack (\s -> AND) }
<0>				"!"			{ pack (\s -> NOT) }
<0>				\>\=			{ pack (\s -> GE) }
<0>				\>			{ pack (\s -> GT) }
<0>				\<\=			{ pack (\s -> LE) }
<0>				\<			{ pack (\s -> LT) }
<0>				\!\=			{ pack (\s -> NEQ) }
<0>				\=			{ pack (\s -> FEQ) }
<0>				\=\=			{ pack (\s -> EQ) }
<0>				\/			{ pack (\s -> DIVIDE) }
<0>				\*			{ pack (\s -> TIMES) }
<0>				\-			{ pack (\s -> MINUS) }
<0>				\+			{ pack (\s -> PLUS) }
<0>				\%			{ pack (\s -> MODE) }
<0>				\}			{ pack (\s -> RBRACE) }
<0>				\{			{ pack (\s -> LBRACE) }
<0>				\[			{ pack (\s -> LBRACK) }
<0>				\]			{ pack (\s -> RBRACK) }
<0>				\)			{ pack (\s -> RPAREN) }
<0>				\(			{ pack (\s -> LPAREN) }
<0>				\;			{ pack (\s -> SEMICOLON) }
<0>				:			{ pack (\s -> COLON) }
<0>				","			{ pack (\s -> COMMA) }
<0>				"."			{ pack (\s -> DOT) }
<0>				\"			{ enterNewString `andBegin` string }
<string>        \\n         { addCharToString '\n' }
<string>        \\t         { addCharToString '\t' }
<string>        \\r         { addCharToString '\r' }
<string>        \\0         { addCharToString '\0' }
<string>        \\a         { addCharToString '\a' }
<string>        \\b         { addCharToString '\b' }
<string>        \\f         { addCharToString '\f' }
<string>        \\\'        { addCharToString '\'' }
<string>        \\\^[@-_]   { addControlToString   }
<string>        \\$digit$digit$digit
                            { addAsciiToString }
<string>        \\\\        { addCharToString '\\' }
<string>        \\\"        { addCharToString '\"' }
<string>        \"          { leaveString `andBegin` 0 }
<string>        .           { addCurrentToString }
<string>        \n          { \_ _ -> lexerError "Illegal character newline" }
<0>				@comment		{ skip }
<0>				@identifier 		{ pack (\s -> ID s) }
<0>				@int			{ pack (\s -> INT $ read s) }
<0>				$whitespace+	;
<0>				.			{\_ _ -> alexError "Illegal character" }


{
-- The user state monad 
type Posn = Maybe AlexPosn

data AlexUserState = AlexUserState
  {
       lexerStringState   :: Bool
    ,  lexerStringValue   :: String
    ,  parserCollIndent   :: Map String Int
    ,  parserCurrentToken :: Token
    ,  parserPos          :: Posn
  }

alexInitUserState :: AlexUserState
alexInitUserState =
  AlexUserState
  {
    lexerStringState   = False
  , lexerStringValue   = ""
  , parserCollIndent   = M.empty
  , parserCurrentToken = EOFToken
  , parserPos          = Nothing
  }

-- For String xxx

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}},())

getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s-> Right(s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}},())

-- Action
-- actions
enterNewString :: AlexInput -> Int -> Alex Token
enterNewString _ _ =
  do setLexerStringState True
     setLexerStringValue ""
     alexMonadScan

addCharToString :: Char -> AlexInput -> Int -> Alex Token
addCharToString c _ _ =
  do addCharToLexerStringValue c
     alexMonadScan


-- if we are given the special form '\nnn'
addAsciiToString :: AlexInput -> Int -> Alex Token
addAsciiToString i@(_, _, input) len = if (v < 256)
                                          then addCharToString c i len
                                          else lexerError ("Invalid ascii value : " ++ input)
  where
    s = if (len == 4)
          then drop 1 input
          else error "Invalid call to 'addAsciiToString'"
    r = readDec s
    v = if (length r == 1)
           then fst (head r)
           else error "Invalid call to 'addAsciiToString'"
    c = chr v


-- if we are given the special form '\^A'
addControlToString :: AlexInput -> Int -> Alex Token
addControlToString i@(_,_, input) len = addCharToString c' i len
  where
    c = if (len == 1)
           then head input
           else error "Invlid call to 'addControlToString'"
    v = ord c
    c' = if (v >= 64)
            then chr (v - 64)
            else error "Invalid call to 'addControlToString'"

addCurrentToString :: AlexInput -> Int -> Alex Token
addCurrentToString i@(_, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
        then head input
        else error "Invalid call to addCurrentToString''"

leaveString :: AlexInput -> Int -> Alex Token
leaveString (p@(AlexPn _ line col), _, input) len =
  do s <- getLexerStringValue
     setLexerStringState False
     return (Token (STRING (reverse s)) (SrcPos line col))

-- Error function
lexerError :: String -> Alex a
lexerError msg =
  do (p, c, inp) <- alexGetInput
     let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
     let inp2 = if (length inp1 > 30)
                   then trim (take 30 inp1)
                   else trim inp1
     let disp = if (null inp)
                   then " at end of file"
                   else if (null inp2)
                           then " before end of line"
                           else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
     let disp3 = if (null msg)
                    then "Lexer error"
                    else trim msg
     alexError (disp3 ++ " at " ++ show p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')


-- The token type
alexEOF = return EOFToken

alexGetLine :: Alex Int
alexGetLine
 = Alex $ \s@AlexState{ alex_pos=pos@(AlexPn addr line column) } -> 
    Right (s, line)

alexGetColumn :: Alex Int
alexGetColumn
 = Alex $ \s@AlexState{ alex_pos=pos@(AlexPn addr line column) } -> 
    Right (s, column)    


-- helper function
pack :: (String -> TokenType) -> (AlexInput -> Int -> Alex Token)
pack getToken = \alexInput@((AlexPn _ line col), _, _) len ->
	return $ Token (getToken (getString alexInput len)) (SrcPos line col)
	where
		getString (_, prevChar, str) len = take len str

packToken :: Token -> (AlexInput -> Int -> Alex Token)
packToken token = \alexInput len -> return token

getPos :: Token -> SrcPos
getPos (Token _ pos) = pos

tackLexer :: (Token -> Alex a) -> Alex a
tackLexer cont = do
		token <- alexMonadScan
		cont token
		


}
