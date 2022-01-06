module IR where

import Parser

type Temp  = String
type Label = String

data Instr = MOVE Temp Temp 
           | MOVEI Temp Int
           | OP BinaryOperator Temp Temp Temp
           | OPI BinaryOperator Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp BinaryOperator Temp Label Label
           | CALL Temp Label [Temp]
           | RETURN Temp
           | FUN Label [Temp] [Instr]
           deriving (Eq, Show)

