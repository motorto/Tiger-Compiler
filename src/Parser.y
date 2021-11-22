-- vim: filetype=haskell 

-- TODO: 
--  * let var-decl-list in expr-seq end 

{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token

-- reserved
if { Token_If }
break { Token_Break }
do { Token_Do }
else { Token_Else }
end { Token_End }
for { Token_For }
function { Token_Function }
in { Token_In }
let { Token_Let }
of { Token_Of }
then { Token_Then }
var { Token_Var }
while { Token_While }
to { Token_To }
print{ Token_Print }
printi{ Token_Printi }
scani{ Token_Scani }

-- punctuations signs
',' { Token_Comma }
':' { Token_Collon }
';' { Token_Semi_Collon }
'(' { Token_Lparenth }
')' { Token_Rparenth }
'[' { Token_Lsquare_Parenth }
']' { Token_Rsquare_Parenth }

-- operators
'+' { Token_Plus }
'-' { Token_Minus }
'*' { Token_Times }
'/' { Token_Divided }
'%' { Token_Mod }
'=' { Token_Equal }
'<>' { Token_Not_Equals }
'<' { Token_Less_Then }
'>' { Token_Bigger_Then }
'>=' { Token_Bigger_Or_Equal_Then }
'<=' { Token_Less_Or_Equal_Then }
'&' { Token_And }
'|' { Token_Or }
':=' { Token_Assign }

-- Types 
int { Token_Int $$ }
true     { Token_Boolean_True $$ }
false    { Token_Boolean_False $$ }
identifier { Token_Identifier $$ }
string { Token_String $$ }

-- Precedences 
%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'

%% --Grammar

Expr : int { Int $1 }
     | string { String $1 }
     | Expr '+' Expr { Op Add $1 $3 }
     | Expr '-' Expr { Op Subtraction $1 $3 }
     | Expr '*' Expr { Op Multiplication $1 $3 } 
     | Expr '/' Expr { Op Division $1 $3 } 
     | Expr '%' Expr { Op Module $1 $3 } 
     | Expr '=' Expr { Op Equals $1 $3 } 
     | Expr '<>' Expr { Op NotEquals $1 $3 } 
     | Expr '<' Expr { Op Less $1 $3 } 
     | Expr '<=' Expr { Op LessEquals $1 $3 } 
     | Expr '>' Expr { Op Bigger $1 $3 } 
     | Expr '>=' Expr { Op BiggerEquals $1 $3 } 
     | Expr '&' Expr { Op And $1 $3 } 
     | Expr '|' Expr { Op Or $1 $3 } 
     | '-'Expr {Negative $2} 
     | identifier'('ExprList')' {FuncCall $1 $3}
     | '('ExprSeq')' {ExpSeq $2 }
     | LValue ':=' Expr {Assign $1 $3}
     | if Expr then Expr {If $2 $4}
     | if Expr then Expr else Expr {IfThen $2 $4 $6} 
     | while Expr do Expr {While $2 $4 }
     | break {Break }
     | scani '(' ')' { ScanI }
     | printi '(' Expr ')' { PrintI $3}
     | print '(' Expr ')' { Print $3}
     | let '(' Vardeclist ')' in '(' ExprSeq ')' end { Vardeclist $3 $7} 

Vardeclist : {-empty -} { [] } --acho que nao pode ter empty
	| Vardecl { $1 }
	| Vardeclist Vardecl { $2 : $1 }


ExprSeq : {- empty -} { [] }
        | Expr { [$1] }
        | ExprSeq ';' Expr { $3 : $1 }

LValue : identifier {Var $1}

ExprList : {- empty -} { [] }
         | Expr { [$1] }
         | ExprList ',' Expr { $3 : $1 }

{

-- type Lvalue = String

data Expr 
        = Int Int 
        | String String
        | Op BinaryOperator Expr Expr
        | Negative Expr
        | FuncCall String [Expr]
        | ExpSeq [Expr]
        | Assign LValue Expr
        | ScanI 
        | PrintI Expr
        | Print Expr
        | If Expr Expr
        | IfThen Expr Expr Expr
        | While Expr Expr
        | Break
        deriving Show

data LValue = Var String
        deriving Show

data BinaryOperator 
        = Add 
        | Subtraction
        | Multiplication
        | Division
        | Module
        | Equals
        | NotEquals
        | Less 
        | LessEquals
        | Bigger 
        | BiggerEquals 
        | And 
        | Or 
        deriving Show

parseError :: [Token] -> a
parseError toks = error ("parse error" ++ show toks)
}
