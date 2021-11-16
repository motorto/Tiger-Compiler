-- vim: filetype=haskell 

{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token

-- reserved 
if { IF }
break { BREAK }
do { DO }
else { ELSE }
end { END }
for { FOR }
function { FUNCTION }
in { IN }
let { LET }
of { OF }
then { THEN }
var { VAR }
while { WHILE }
to { TO }

-- punctuations signs
',' { COMMA }
':' { COLLON }
';' { SEMI_COLLON }
'(' { LPARENTH }
')' { RPARENTH }
'[' { LSQUARE_PARENTH }
']' { RSQUARE_PARENTH }

-- operators
'+' { PLUS }
'-' { MINUS }
'*' { TIMES }
'/' { DIVIDED }
'%' { MOD }
'=' { EQUAL }
'<>' { NOT_EQUALS }
'<' { LESS_THEN }
'>' { BIGGER_THEN }
'>=' { BIGGER_OR_EQUAL_THEN }
'<=' { LESS_OR_EQUAL_THEN }
'&' { AND }
'|' { OR }
':=' { ASSIGN }

-- Types
num { NUM $$ }
id {ID $$}
string {STRING $$}

-- Precedences 
%nonassoc '<' '>' '<=' '>='
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
