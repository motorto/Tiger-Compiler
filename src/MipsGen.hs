{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MipsGen where

import CodeGen
import IR
import Parser

printPlease :: [String] -> String
printPlease [] = "\n"
printPlease (x:xs) = x ++ "\n" ++ printPlease xs

start :: [Instr] -> [String]
start instruction =  start' instruction ++ ioFunctions "printi" ++ ioFunctions "scani"

start' :: [Instr] -> [String]
start' [] = []
start' (first : rest) =
  let code1 = transToMips first
      code2 = start' rest
   in code1 ++ code2

ioFunctions :: String -> [String]
ioFunctions function = case function of
  "printi" -> transToMips (LABEL "printi") ++ ["\tli $v0, 1", "\tlw $a0, 0($sp)", "\tsyscall", "\tjr $ra"]
  "scani" -> transToMips (LABEL "scani") ++ ["\tli $v0, 5", "\tsyscall", "\tjr $ra"]

transToMips :: Instr -> [String]
transToMips instruction = case instruction of
  (MOVE t1 t2) -> ["\tmove " ++ t1 ++ " , " ++ t2]
  (MOVEI t1 i) -> ["\tli " ++ t1 ++ " , " ++ show i]
  (OP op t1 t2 t3) -> case op of
    Add -> ["\tadd " ++ t1 ++ " , " ++ t2 ++ " , " ++ t3]
    Subtraction -> ["\tsub " ++ t1 ++ " , " ++ t2 ++ " , " ++ t3]
    Multiplication -> ["\tmult " ++ t2 ++ " , " ++ t3, "\tmflo " ++ t1]
    Division -> ["\tdiv " ++ t2 ++ " , " ++ t3, "\tmflo " ++ t1]
    Module -> ["\tdiv " ++ t2 ++ " , " ++ t3, "\tmfhi " ++ t1]
  (OPI op t1 t2 i) -> case op of
    Add -> ["\taddi " ++ t1 ++ " , " ++ t2 ++ " , " ++ show i]
    Subtraction -> ["\tsub " ++ t1 ++ " , " ++ t2 ++ " , " ++ show i]
    Multiplication -> ["\tmult " ++ t2 ++ " , " ++ show i ++ " , ", "\tmflo " ++ t1]
    Division -> ["\tdiv " ++ t2 ++ " , " ++ show i, "\tmflo " ++ t1]
    Module -> ["\tdiv " ++ t2 ++ " , " ++ show i, "\tmfhi " ++ t1]
  (LABEL l1) -> [l1 ++ ": "]
  (JUMP l1) -> ["\tj " ++ l1]
  (COND t1 op t2 l1 l2) -> case op of
    Less -> ["\tblt " ++ t1 ++ " , " ++ t2 ++ " , " ++ l1, "\tj " ++ l2]
    LessEquals -> ["\tbgt " ++ t1 ++ " , " ++ t2 ++ " , " ++ l2, "\tj " ++ l1]
    Bigger -> ["\tbgt " ++ t1 ++ " , " ++ t2 ++ " , " ++ l1, "\tj " ++ l2]
    BiggerEquals -> ["\tblt " ++ t1 ++ " , " ++ t2 ++ " , " ++ l2, "\tj " ++ l1]
    Equals -> ["\tbne " ++ t1 ++ " , " ++ t2 ++ " , " ++ l2, "\tj " ++ l1]
    NotEquals -> ["\tbeq " ++ t1 ++ " , " ++ t2 ++ " , " ++ l2, "\tj " ++ l1]
  (CALL t name args) -> callFunction name args 0 ++ transToMips (MOVE t "$v0")
  (FUN name args code) -> transToMips (LABEL name) ++ ["sw $fp, -4($sp)", "sw $ra, -8($sp)", "la $fp, 0($sp)"] ++ start' code
  (RETURN t) -> ["move $v0, " ++ show t, "la $sp, 0($fp)", "lw $ra, -8($sp)", "lw $fp, -4($sp)", "jr $ra"]

callFunction :: Label -> [Temp] -> Int -> [String]
callFunction name [] offset =
  ["la " ++ "$sp, " ++ show offset ++ "($sp)", "jal " ++ name, "la $sp, " ++ show (offset * (-1)) ++ "($sp)"]
callFunction name (a : args) oldOffset =
  let newOffset = oldOffset - 4
      storeArg1 = ["sw " ++ a ++ " , " ++ show newOffset ++ "($sp)"]
      restArgs = callFunction name args newOffset
   in storeArg1 ++ restArgs
