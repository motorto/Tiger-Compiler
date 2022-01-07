{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MipsGen where

import CodeGen
import IR
import Parser

start :: [Instr] -> [String]
start [] = []
start (first : rest) =
  let code1 = transToMips first
      code2 = start rest
   in code1 : code2

transToMips :: Instr -> String
transToMips instruction = case instruction of
  (MOVE t1 t2) -> "move" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2
  (MOVEI t1 i) -> "move" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ show i
  (OP op t1 t2 t3) -> case op of
    Add -> "add" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ t3
    Subtraction -> "sub" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ t3
    Multiplication -> "mult" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ t3
    Division -> "div" ++ " " ++ t2 ++ " " ++ "," ++ " " ++ t3 ++ "\n" ++ "mflo" ++ " " ++ t1
    Module -> "div" ++ " " ++ t2 ++ " " ++ "," ++ " " ++ t3 ++ "\n" ++ "mfhi" ++ " " ++ t1
  (OPI op t1 t2 i) -> case op of
    Add -> "addi" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ show i
    Subtraction -> "sub" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ show i
    Multiplication -> "mult" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ show i
    Division -> "div" ++ " " ++ t2 ++ " " ++ "," ++ " " ++ show i ++ "\n" ++ "mflo" ++ " " ++ t1
    Module -> "div" ++ " " ++ t2 ++ " " ++ "," ++ " " ++ show i ++ "\n" ++ "mfhi" ++ " " ++ t1
  (LABEL l1) -> l1 ++ " " ++ ":" ++ " "
  (JUMP l1) -> "j" ++ " " ++ l1
  (COND t1 op t2 l1 l2) -> case op of
    Less -> "blt" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l1 ++ "\n" ++ "j" ++ " " ++ l2
    LessEquals -> "bgt" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l2 ++ "\n" ++ "j" ++ " " ++ l1
    Bigger -> "bgt" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l1 ++ "\n" ++ "j" ++ " " ++ l2
    BiggerEquals -> "blt" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l2 ++ "\n" ++ "j" ++ " " ++ l1
    Equals -> "bne" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l2 ++ "\n" ++ "j" ++ " " ++ l1
    NotEquals -> "beq" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l2 ++ "\n" ++ "j" ++ " " ++ l1
