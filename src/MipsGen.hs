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
   in code1 ++ code2

transToMips :: Instr -> [String]
transToMips instruction = case instruction of
  (MOVE t1 t2) -> ["move" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2]
  (MOVEI t1 i) -> ["li" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ show i]
  (OP op t1 t2 t3) -> case op of
    Add -> ["add" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ t3]
    Subtraction -> ["sub" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ t3]
    Multiplication -> ["mult" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ t3]
    Division -> ["div" ++ " " ++ t2 ++ " " ++ "," ++ " " ++ t3, "mflo" ++ " " ++ t1]
    Module -> ["div" ++ " " ++ t2 ++ " " ++ "," ++ " " ++ t3, "mfhi" ++ " " ++ t1]
  (OPI op t1 t2 i) -> case op of
    Add -> ["addi" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ show i]
    Subtraction -> ["sub" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ show i]
    Multiplication -> ["mult" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ show i]
    Division -> ["div" ++ " " ++ t2 ++ " " ++ "," ++ " " ++ show i, "mflo" ++ " " ++ t1]
    Module -> ["div" ++ " " ++ t2 ++ " " ++ "," ++ " " ++ show i, "mfhi" ++ " " ++ t1]
  (LABEL l1) -> [l1 ++ ":" ++ " "]
  (JUMP l1) -> ["j" ++ " " ++ l1]
  (COND t1 op t2 l1 l2) -> case op of
    Less -> ["blt" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l1, "j" ++ " " ++ l2]
    LessEquals -> ["bgt" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l2, "j" ++ " " ++ l1]
    Bigger -> ["bgt" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l1, "j" ++ " " ++ l2]
    BiggerEquals -> ["blt" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l2, "j" ++ " " ++ l1]
    Equals -> ["bne" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l2, "j" ++ " " ++ l1]
    NotEquals -> ["beq" ++ " " ++ t1 ++ " " ++ "," ++ " " ++ t2 ++ " " ++ "," ++ " " ++ l2, "j" ++ " " ++ l1]
  (CALL t name args) -> callFunction name args 0 ++ transToMips (MOVE t "$v0")
  (FUN name args code) -> transToMips (LABEL name) ++ ["sw $fp, -4($sp)", "sw $ra, -8($sp)", "la $fp, 0($sp)"] ++ start code
  (RETURN t) -> ["move $v0, " ++ show t, "la $sp, 0($fp)", "lw $ra, -8($sp)", "lw $fp, -4($sp)", "jr $ra"]

callFunction :: Label -> [Temp] -> Int -> [String]
callFunction name [] offset =
  ["la " ++ "$sp, " ++ show offset ++ "($sp)", "jal " ++ name, "la $sp, " ++ show (offset * (-1)) ++ "($sp)"]
callFunction name (a : args) oldOffset =
  let newOffset = oldOffset - 4
      storeArg1 = ["sw " ++ a ++ " , " ++ show newOffset ++ "($sp)"]
      restArgs = callFunction name args newOffset
   in storeArg1 ++ restArgs
