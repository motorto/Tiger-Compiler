module Main where

import Lexer
import Parser

main :: IO ()
main = do
  input <- getContents
  print ( parser $ alexScanTokens input)
