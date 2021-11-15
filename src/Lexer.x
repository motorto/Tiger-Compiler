-- vim: filetype=haskell 

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
if { \_ -> IF }
break { \_ -> BREAK }
do { \_ -> DO }
else { \_ -> ELSE }
end { \_ -> END }
for { \_ -> FOR }
function { \_ -> FUNCTION }
in { \_ -> IN }
let { \_ -> LET }
of { \_ -> OF }
then { \_ -> THEN }
var { \_ -> VAR }
while { \_ -> WHILE }

-- punctuations signs
"," { \_ -> COMMA }
":" { \_ -> COLLON }
";" { \_ -> SEMI_COLLON }
"(" { \_ -> LPARENTH}
")" { \_ -> RPARENTH}
"[" { \_ -> LSQUARE_PARENTH}
"]" { \_ -> RSQUARE_PARENTH}

-- operators
"+" { \_ -> PLUS}
"-" { \_ -> MINUS}
"*" { \_ -> TIMES}
"/" { \_ -> DIVIDED}
"%" { \_ -> MOD}
"=" { \_ -> EQUAL}
"<>" { \_ -> NOT_EQUALS}
"<" { \_ -> LESS_THEN}
">" { \_ -> BIGGER_THEN}
">=" { \_ -> BIGGER_OR_EQUAL_THEN}
"<=" { \_ -> LESS_OR_EQUAL_THEN}
"&" { \_ -> AND}
"|" { \_ -> OR}
":=" { \_ -> ASSIGN}

-- comments
"//".* ;
"/*"(\s|\n|.)*"*/" ;

-- Types 
$digit+ { \s -> NUM (read s) }
$alpha($alpha|$digit)* { \s -> ID s }
\".*\" { \s -> STRING s}

{
data Token
  = NUM Int 
  | IF
  | BREAK 
  | DO 
  | ELSE 
  | END 
  | FOR 
  | FUNCTION 
  | IN 
  | LET 
  | OF 
  | THEN 
  | VAR 
  | WHILE 
  | COMMA 
  | COLLON
  | SEMI_COLLON
  | LPARENTH 
  | RPARENTH
  | LSQUARE_PARENTH 
  | RSQUARE_PARENTH
  | PLUS
  | MINUS
  | TIMES
  | DIVIDED
  | MOD
  | EQUAL
  | NOT_EQUALS
  | LESS_THEN
  | BIGGER_THEN
  | BIGGER_OR_EQUAL_THEN 
  | LESS_OR_EQUAL_THEN
  | AND
  | OR 
  | ASSIGN
  | ID String 
  | STRING String 
  deriving (Eq, Show)
}

