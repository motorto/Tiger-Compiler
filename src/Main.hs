module Main where

import Lexer
-- import Parser

main :: IO ()
main = do
  -- print (Parser $ alexScanTokens input)
  input <- getContents
  print (alexScanTokens input)
