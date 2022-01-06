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
  print ast
  let code = evalState (transProgram ast Map.empty) (0,0)
  print code
