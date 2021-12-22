module CodeGen where

import           Parser
import           IR
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State

type Table = Map Identifier String
type Count = (Int,Int)

{- 
 - String (optional)
 - Negative
 - letin
 - ExpSeq  (line 94) -- Verify
 - Break (optional)
 -}

-- translate an expression
transExpr :: Table -> Expr -> Identifier -> State Count [Instr]
transExpr tabl (Var (VarName x)) dest
  = case Map.lookup x tabl of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "invalid variable"

transExpr tabl (Number n) dest 
  = return [MOVEI dest n]

transExpr tabl (Op op e1 e2) dest
  = do temp1 <- newTemp 
       temp2 <- newTemp 
       code1 <- transExpr tabl e1 temp1 
       code2 <- transExpr tabl e2 temp2
       popTemp 2
       return (code1 ++ code2 ++ [OP op dest temp1 temp2])

-- Translate Boolean Conditions
transCond :: Table -> Expr -> Label -> Label -> State Count [Instr]
transCond tabl (Cond rel e1 e2) ltrue lfalse 
  | rel == Lt || rel == Lteq || rel == Eq =
      do temp1 <- newTemp
         temp2 <- newTemp 
         code1 <- transExpr tabl e1 temp1
         code2 <- transExpr tabl e2 temp2
         popTemp 2
         return ( code1 ++ code2 ++ [COND temp1 rel temp2 ltrue lfalse] )

-- Translate Statements
transStm :: Table -> Stm -> State Count [Instr]
transStm tabl (Assign var expr) 
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable"
      Just dest -> return (transExpr expr table dest)

transStm tabl (ExpSeq stms)
  = do code1 <- transBlock tabl stms
       return (code1)

transExpr::Table -> [Exp] -> State Count ([Instr],[Temp])
transExpr tabl [] = return ([],[])
transExpr tabl (expr:exprs) = do temp1 <- newTemp
                                 code1 <- transExpr tabl expr temp1
                                 popTemp(1)
                                 (code2,temps) <- transExpr tabl exprs
                                 return (code1 ++ code2,[temp1] ++ temps)

transStm tabl (IfThen cond stm1) 
  = do ltrue  <- newLabel 
       lfalse <- newLabel 
       code0  <- transCond tabl cond ltrue lfalse 
       code1  <- transStm tabl stm1
       return (code0 ++ [LABEL ltrue] ++
               code1 ++ [LABEL lfalse])

transStm tabl (IfThenElse cond stm1 stm2) 
  = do ltrue <- newLabel 
       lfalse <- newLabel 
       lend <- newLabel 
       code0 <- transCond tabl cond ltrue lfalse 
       code1 <- transStm tabl stm1 
       code2 <- transStm tabl stm2 
       return (code0 ++ [LABEL ltrue] ++ code1 ++
               [JUMP lend, LABEL lfalse] ++ code2 ++ [LABEL lend])

transStm tabl (While cond stm) =
  do label1 <- newLabel
     label2 <- newLabel
     label3 <- newLabel
     code1 <- transStm tabl stm
     code2 <- transCond tabl cond lbody lend
     return ([LABEL label1 ] ++ code1 ++
              [LABEL label2 ] ++ code2 ++ [JUMP label1 , LABEL label3 ])

-- Verify this
transStm tabl (FuncCall func expr)
    = do (code1,temps) <- transExpr tabl expr
       return (code1 ++ [CALL func (temps)])


transBlock:: Table -> [Stm] -> State Count [Instr]
transBlock tabl [] = return []
transBlock tabl (first:rest)
  = do code1 <- transStm tabl first
       code2 <- transBlock tabl rest
       return (code1 ++ code2)

transExpr tabl (FuncCall id args) dest
  = do (code, temps) <- transArgs tabl args
       popTemp (length args)
       return (code ++ [CALL dest id temps])

transArgs tabl args  = worker args 
  where
    worker []  = return ([], [])
    worker (exp:exps) 
      = do temp <- newTemp 
           code <- transExpr tabl exp temp 
           (code', temps') <- worker exps 
           return (code++code', temp:temps')

newTemp :: State Count Temp
newTemp = do (t,l)<-get; put (t+1,l); return ("t"++show t)

newLabel :: State Count Label 
newLabel = do (t,l)<-get; put (t,l+1); return ("L"++show l)

popTemp :: Int -> State Count ()
popTemp k =  modify (\(t,l) -> (t-k,l))
