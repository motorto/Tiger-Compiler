module CodeGen where

import           Parser
import           IR
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State

type Table = Map Identifier String

type Count = (Int,Int)  -- counter for temps and labels

newTemp :: State Count Temp
newTemp = do (t,l)<-get; put (t+1,l); return ("t"++show t)

popTemp :: Int -> State Count ()
popTemp k =  modify (\(t,l) -> (t-k,l))

newLabel :: State Count Label 
newLabel = do (t,l)<-get; put (t,l+1); return ("L"++show l)

{-
 - Traduzir Expressões 
 
Expr : num { Number $1 } [X]
     | stringContent { BuildString $1 } [Opt]
     | LValue { Var $1} [X]
     | Expr x  Expr { Op x $1 $3 } [X]
     | Expr x Expr { Cond x $1 $3 }  [X] -- TransCond
     | '-'Expr {Negative $2}  [?]
     | identifier '(' ExprList ')' { FuncCall $1 $3} [X]
-}

transExp :: Table -> Expr -> Identifier -> State Count [Instr]

transExp tabl (Number n) dest
  = return [MOVEI dest n]

transExp tabl (Var (Varname x) dest
  = case Map.lookup x tabl of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "invalid variable"

transExp tabl (Negative e1) dest
  = do place1 <- newTemp 
       code1 <- transExp tabl e1 place1
       return (code1 ++ [NEGATIVE dest place1 ])

transExp tabl (Op op e1 e2) dest
  = do place1 <- newTemp 
       place2 <- newTemp 
       code1 <- transExp tabl e1 place1
       code2 <- transExp tabl e2 place2
       -- popTemp 2 -- ????
       return (code1 ++ code2 ++ [OP op dest place1 place2])


transExp tabl (FuncCall id exps ) dest
  = do (code1, temps) <- TransExps tabl exps
       -- popTemp (length exps)
       return (code1 ++ [CALL dest id temps])

transExpressions tabl exps = worker exps 
  where
    worker []  = return ([], [])
    worker (expHead:expTail) 
      = do temp1 <- newTemp 
           code1 <- transExp tabl expHead temp1
           (code2, temp2) <- worker expTail
           return (code1 ++ code2, temp1:temp2)

{-
   '('ExprSeq')' { ExpSeq $2 } [?]
   if Expr then Expr {IfThen $2 $4} [X]
   if Expr then Expr else Expr {IfThenElse $2 $4 $6} [X]
   while Expr do Expr {While $2 $4 } [X]
   LValue ':=' Expr {Assign $1 $3} [?]
   break { Break } []
   let VarDecList in ExprSeq end { LetIn $2 $4} []
 -}

transStat :: Table -> Stm -> State Count [Instr]
transStat (ExpSeq stat1:statTail)
      = do code1 <- transExp tabl stat1 
           code2 <- transExp tabl statTail
           return (code1 ++ code2)

transStat (IfThen cond stat1)
      = do label1 <- newLabel 
        label2 <- newLabel 
        code1 <- transCond tabl cond label1 label2
        code2 <- transStat tabl stat1
        return (code1 ++ [LABEL label1] ++
                code2 ++ [LABEL false])

transStat (IfThenElse cond stat1 stat2)
      = do label1 <- newLabel 
        label2 <- newLabel 
        label3 <- newLabel 
        code1 <- transCond tabl cond label1 label2
        code2 <- transStat tabl stat1
        code3 <- transStat tabl stat2 
        return (code1 ++ [LABEL label1] ++
                code2 ++ [JUMP label3 , LABEL label2] ++ 
                code3 ++ [LABEL label3])

transStat (While cond stat1)
      = do label1 <- newLabel 
        label2 <- newLabel 
        label3 <- newLabel
        code1 <- transCond tabl cond label2 label3
        code2 <- transStat tabl stat1
        return ([LABEL label1] ++ code1 ++
                [LABEL label2] ++ code2 ++ 
                [JUMP label, LABEL label3])

transStat (Assing id exp1) 
     = case Map.lookup id tabl of
        Nothing -> error "undefined variable"
        Just dest -> do temp1 <- newTemp 
                      code1 <- transExp tabl exp1 temp1
                      return (code1 ++ [MOVE dest temp1])


transCond :: Table -> Expr -> Label -> Label -> State Count [Instr]
transCond tabl (Op rel e1 e2) ltrue lfalse
    = do t1 <- newTemp
         t2 <- newTemp
         code1 <- transExp tabl exp1 t1
         code2 <- transExp tabl exp2 t2
         return (code1 ++ code2 ++ 
                [COND t1 rel t2 ltrue lfalse])

transExp tabl (Cond op e1 e2) ltrue lfalse
  = do t1 <- newTemp 
       t2 <- newTemp 
       code1 <- transExp tabl e1 t1
       code2 <- transExp tabl e2 t2
       -- popTemp 2 -- ????
       return (code1 ++ code2 ++ [COND t1 rel t2 ltrue lfalse])
