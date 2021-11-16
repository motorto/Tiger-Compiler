-- vim: filetype=haskell 

{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token

num { NUM $$ }
'+' { PLUS }
'-' { MINUS }
'*' { TIMES }
'/' { DIVIDED }

-- Precedences 

%left '+' '-'
%left '*' '/'

%% --Grammar

Start : Exp { $1 }

Exp : num { Num $1 }
    | Exp '+' Exp { Add $1 $3}
    | Exp '-' Exp { Sub $1 $3}
    | Exp '*' Exp { Mult $1 $3}
    | Exp '/' Exp { Div $1 $3}

{

data Start = Exp 
           deriving Show

data Exp = Num Int 
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         deriving Show

parseError :: [Token] -> a
parseError toks = error "parse error"
}
