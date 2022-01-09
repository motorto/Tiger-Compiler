module Main where

import CodeGen
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Lexer
import MipsGen
import Parser

main :: IO ()
main = do
  input <- getContents
  let ast = parser (alexScanTokens input)
  writeFile "ast.txt" $ show ast ++ "\n"
  let code = evalState (transProgram ast) (0, 0)
  writeFile "3addr.txt" $ show code ++ "\n"
  let mips = printPlease $ start code
  writeFile "mips.asm" mips
  putStr mips
