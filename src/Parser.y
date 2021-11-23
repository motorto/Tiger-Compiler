-- vim: filetype=haskell 

-- TODO: 
-- Identifiers dentro das expressões

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
int { Token_Type_Integer }
string { Token_Type_String }

num { Token_Number $$ }
stringContent { Token_String $$ }

true     { Token_Boolean_True $$ }
false    { Token_Boolean_False $$ }
identifier { Token_Identifier $$ }

-- Precedences 
%nonassoc ':='
%left '|' '&' 
%nonassoc '<' '>' '<=' '>=' '<>' '='
%left '+' '-'
%left '*' '/' '%'

%% --Grammar

Program : let DecList in ExprSeq {Begin $2 $4} 

DecList : Decl { [$1] }
        | DecList Decl  {$2 : $1}

Decl : VarDecl { VarDecla $1}
     | FuncDecl { FunDecla $1}

FuncDecl : function identifier'('TypeFields')' '=' Expr { FunctionDeclare $2 $4 $7}
         | function identifier'('TypeFields')'':' TypeId '=' Expr { FunctionDeclareTyped $2 $4 $7 $9}

TypeFields : TypeField {[$1]}
           | TypeFields ',' TypeField {$3 : $1}

TypeField : identifier ':' TypeId {Declare $1 $3}

TypeId : int {TypeInt}
       | string {TypeString}

Expr : num { Number $1 }
     | LValue ':=' Expr {Assign $1 $3}
     | stringContent { BuildString $1 }
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
     | identifier '(' ExprList ')' {FuncCall $1 $3}
     | '('ExprSeq')' {ExpSeq $2 }
     | if Expr then Expr {If $2 $4}
     | if Expr then Expr else Expr {IfThen $2 $4 $6} 
     | while Expr do Expr {While $2 $4 }
     | break {Break }
     | scani '(' ')' { ScanI }
     | printi '(' Expr ')' { PrintI $3}
     | print '(' Expr ')' { Print $3}
     | let VarDecList in ExprSeq end {LetIn $2 $4}

VarDecList : VarDecl { [$1] }
           | VarDecList VarDecl { $2 : $1 }

VarDecl : var identifier ':=' Expr { Decl $2 $4 }

ExprSeq : {- empty -} { [] }
        | Expr { [$1] }
        | ExprSeq ';' Expr { $3 : $1 }

LValue : identifier {Var $1}

ExprList : {- empty -} { [] }
         | Expr { [$1] }
         | ExprList ',' Expr { $3 : $1 }

{

data Program = Begin [Decl] [Expr]
            deriving Show

data Decl = VarDecla VarDecl
          | FunDecla FuncDecl
            deriving Show

data FuncDecl = FunctionDeclare String [TypeField] Expr
              | FunctionDeclareTyped String [TypeField] TypeId Expr
            deriving Show

data TypeField = Declare String TypeId
            deriving Show

data TypeId = TypeInt
            | TypeString
            deriving Show

data Expr 
        = Number Int 
        | BuildString String
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
        | LetIn [VarDecl] [Expr]
        deriving Show

data VarDecl = Decl String Expr
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
