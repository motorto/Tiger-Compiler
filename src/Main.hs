module Main where

import Lexer
import Parser
import CodeGen

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

main :: IO ()
main = do
  input <- getContents
  let ast = parser (alexScanTokens input)
  writeFile "ast.txt" $ show ast ++ "\n"
  let code = evalState (transProgram ast) (0,0)
  print code
