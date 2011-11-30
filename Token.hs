module Token where
import Prelude hiding (GT, LT, EQ)
import SrcPos

-- The token type
data Token = Token TokenType SrcPos 
           | EOFToken
	
-- The Token type:
data TokenType = 
    ID		String
  | INT		Int
  | STRING	String
  | COMMA
  | COLON
  | DOT
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODE
  | EQ
  | FEQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  | AND
  | OR
  | ASSIGN
  | IF
  | ELSE
  | IN
  | FUNCTION
  | NULL
  | TBOOL
  | TVOID
  | TSTRING
  | TINT
  | ARROW
  | NOT
  | WHILE
  | FOR
  | FALSE
  | TRUE
  deriving (Show, Eq)


