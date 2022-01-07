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
   in code1:code2

transToMips :: Instr -> String
transToMips instruction = case instruction of
  (MOVE t1 t2) -> "move" ++ show t1 ++ "," ++ show t2
  (MOVEI t1 i) -> "move" ++ show t1 ++ "," ++ show i
  (OP op t1 t2 t3) -> case op of
    Add -> "add" ++ show t1 ++ "," ++ show t2 ++ "," ++ show t3
    Subtraction -> "sub" ++ show t1 ++ "," ++ show t2 ++ "," ++ show t3
    Multiplication -> "mult" ++ show t1 ++ "," ++ show t2 ++ "," ++ show t3
    Division -> "div" ++ show t2 ++ "," ++ show t3 ++ "\n" ++ "mflo" ++ show t1
    Module -> "div" ++ show t2 ++ "," ++ show t3 ++ "\n" ++ "mfhi" ++ show t1
  (OPI op t1 t2 i) -> case op of
    Add -> "addi" ++ show t1 ++ "," ++ show t2 ++ "," ++ show i
    Subtraction -> "sub" ++ show t1 ++ "," ++ show t2 ++ "," ++ show i
    Multiplication -> "mult" ++ show t1 ++ "," ++ show t2 ++ "," ++ show i
    Division -> "div" ++ show t2 ++ "," ++ show i ++ "\n" ++ "mflo" ++ show t1
    Module -> "div" ++ show t2 ++ "," ++ show i ++ "\n" ++ "mfhi" ++ show t1
