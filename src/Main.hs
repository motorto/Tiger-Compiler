module Main where

import Lexer
import Parser

main :: IO ()
main = do
  input <- getContents
  let ast <- parser $ alexScanTokens input)
  print (evalState (transProgram ast Map.empty) (0,0))
