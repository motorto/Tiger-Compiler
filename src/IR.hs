{-
  3-Addresss Intermediate code
-}

module IR where

import Parser

type Temp  = String
type Label = String

data Instr = MOVE Temp Temp 
           | MOVEI Temp Int
           | NEGATIVE Temp Temp
           | OP BinOp Temp Temp Temp
           | OPI BinOp Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp RealOp Temp Label Label
           | CALL Temp Label [Temp]
           | RETURN Temp
           deriving (Eq, Show)

