-- vim: filetype=haskell 
--
{
  module Lexer where
}

%wrapper "basic"

-- Regex Expressions
-- $white = [\ \t\n\r\"\\]
$white = [\ \t\n\r\s]
$digit = [0-9]
$alpha = [_a-zA-Z]

tokens :-

$white+ ; --ignore white chars

-- reserved
if { \_ -> Token_If }
break { \_ -> Token_Break }
do { \_ -> Token_Do }
else { \_ -> Token_Else }
end { \_ -> Token_End }
for { \_ -> Token_For }
function { \_ -> Token_Function }
in { \_ -> Token_In }
let { \_ -> Token_Let }
of { \_ -> Token_Of }
then { \_ -> Token_Then }
var { \_ -> Token_Var }
while { \_ -> Token_While }
to { \_ -> Token_To }
print{ \_ -> Token_Print }
printi{ \_ -> Token_Printi }
scani{ \_ -> Token_Scani }

-- punctuations signs
"," { \_ -> Token_Comma }
":" { \_ -> Token_Collon }
";" { \_ -> Token_Semi_Collon }
"(" { \_ -> Token_Lparenth }
")" { \_ -> Token_Rparenth }
"[" { \_ -> Token_Lsquare_Parenth }
"]" { \_ -> Token_Rsquare_Parenth }

-- operators
"+" { \_ -> Token_Plus }
"-" { \_ -> Token_Minus }
"*" { \_ -> Token_Times }
"/" { \_ -> Token_Divided }
"%" { \_ -> Token_Mod }
"=" { \_ -> Token_Equal }
"<>" { \_ -> Token_Not_Equals }
"<" { \_ -> Token_Less_Then }
">" { \_ -> Token_Bigger_Then }
">=" { \_ -> Token_Bigger_Or_Equal_Then }
"<=" { \_ -> Token_Less_Or_Equal_Then }
"&" { \_ -> Token_And }
"|" { \_ -> Token_Or }
":=" { \_ -> Token_Assign }

-- comments
"//".* ;
"/*"(\s|\n|.)*"*/" ;

-- Types 
$digit+ { \s -> Token_Int (read s) }
true { \s -> Token_Boolean_True True }
false { \s -> Token_Boolean_False False }
$alpha($alpha|$digit)* { \s -> Token_Identifier s }
\".*\" { \s -> Token_String (read s)}

{
data Token
  = Token_If
  | Token_Break
  | Token_Do
  | Token_Else
  | Token_End
  | Token_For
  | Token_Function
  | Token_In
  | Token_Let
  | Token_Of
  | Token_Then
  | Token_Var
  | Token_While
  | Token_To
  | Token_Print
  | Token_Printi
  | Token_Scani
  | Token_Comma
  | Token_Collon
  | Token_Semi_Collon
  | Token_Lparenth
  | Token_Rparenth
  | Token_Lsquare_Parenth
  | Token_Rsquare_Parenth
  | Token_Plus
  | Token_Minus
  | Token_Times
  | Token_Divided
  | Token_Mod
  | Token_Equal
  | Token_Not_Equals
  | Token_Less_Then
  | Token_Bigger_Then
  | Token_Bigger_Or_Equal_Then
  | Token_Less_Or_Equal_Then
  | Token_And
  | Token_Or
  | Token_Assign
  | Token_Int Int
  | Token_Boolean_True Bool
  | Token_Boolean_False Bool
  | Token_Identifier String
  | Token_String String
  deriving (Eq, Show)
}

