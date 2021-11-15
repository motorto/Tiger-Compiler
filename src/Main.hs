module Main where

import Lexer

main :: IO ()
main = do
  passed <- getContents
  print (alexScanTokens passed)
