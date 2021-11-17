-- vim: filetype=haskell 

-- TODO: 
--  * Numeros Negativos
--  * While ciclo
--  * ExpList 
--  * ExpSeq -- não esquecer dos parentises if(2<4 , 3 < 5)
--  * let var-decl-list in expr-seq end 
--    * Precisamos de  implementar o ExpList antes

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
%left '*' '/' '%'

%% --Grammar

Start : Exp { $1 }

Exp : num { Num $1 }
    | string { Str $1 }
    | Exp '+' Exp { Add $1 $3}
    | Exp '-' Exp { Sub $1 $3}
    | Exp '*' Exp { Mult $1 $3}
    | Exp '/' Exp { Div $1 $3}
    | Exp '%' Exp { Mod $1 $3}
    | Exp '=' Exp { Equal $1 $3}
    | Exp '<>' Exp { NotEquals $1 $3}
    | Exp '<' Exp { LessThen $1 $3}
    | Exp '>' Exp { BiggerThen $1 $3}
    | Exp '>=' Exp { BiggerEqualThen $1 $3}
    | Exp '<=' Exp { LessEqualThen $1 $3}
    | Exp '&' Exp { And $1 $3}
    | Exp '|' Exp { Or $1 $3}
    | '('Exp')' { $2 }
    | LValue { Var $1 }
    | LValue ':=' Exp { Assign $1 $3 }
    | if Exp then Exp { If $2 $4 }
    | if Exp then Exp else Exp  { IfElse $2 $4 $6 }
    | while Exp do Exp { While $2 $4 }

LValue : id { $1 }

{

type LValue = String 

data Start = Exp 
           deriving Show

data Exp = Num Int
         | Str String
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Equal Exp Exp
         | NotEquals Exp Exp
         | LessThen Exp Exp
         | BiggerThen Exp Exp
         | BiggerEqualThen Exp Exp
         | LessEqualThen Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Assign LValue Exp
         | Var LValue
         | If Exp Exp
         | IfElse Exp Exp Exp
         | While Exp Exp
         deriving Show

parseError :: [Token] -> a
parseError toks = error ("parse error" ++ show toks)
}
