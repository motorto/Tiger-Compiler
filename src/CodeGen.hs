module CodeGen where

{- acho que só falta isto: 
     STM :
     | let VarDecList in ExprSeq end { LetIn $2 $4}

     Inicio do Programa:
     | let DecList in ExprSeq {Begin $2 $4} 
-}

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

transExpression :: Expr -> Table -> Identifier -> State Count [Instr]
transExpression expression tabl dest = case expression of 
                (Number n) -> return [MOVEI dest n]
                (Var x) -> case Map.lookup x tabl of
                                Just temp -> return [MOVE dest temp]
                                Nothing -> error "invalid variable"
                (Op op exp1 exp2) 
                          -> do t1 <- newTemp 
                                t2 <- newTemp 
                                code1 <- transExpression exp1 tabl t1
                                code2 <- transExpression exp2 tabl t2
                                popTemp 2
                                return (code1 ++ code2 ++ [OP op dest t1 t2])
                (Negative exp1) 
                           -> do t1 <- newTemp 
                                 code1 <- transExpression exp1 tabl t1 
                                 popTemp 1
                                 return (code1 ++ [OP Subtraction t1 "0" t1]) 
                (FuncCall id args)
                           -> do (code, temps) <- transArguments args tabl
                                 popTemp (length args)
                                 return (code ++ [CALL dest id temps])
                (ExpSeq args) 
                          -> do (code,temps) <- transArguments args tabl
                                popTemp (length args)
                                return (code)

transStatements :: Expr -> Table -> State Count [Instr] 
transStatements statement tabl = case statement of 
                (Assign x expr) -> case Map.lookup x tabl of
                                  Nothing -> error "undefined variable"
                                  Just dest -> do t1 <- newTemp 
                                                  code1 <- transExpression expr tabl t1
                                                  return (code1 ++ [MOVE dest t1])
                (IfThen cond exp1) 
                          -> do l1 <- newLabel
                                l2 <- newLabel
                                code1 <- transCondition cond tabl l1 l2
                                code2 <- transStatements exp1 tabl 
                                return (code1 ++ [LABEL l1] ++
                                        code2 ++ [LABEL l2])
                (IfThenElse cond exp1 exp2) 
                          -> do l1 <- newLabel
                                l2 <- newLabel
                                l3 <- newLabel
                                code1 <- transCondition cond tabl l1 l2
                                code2 <- transStatements exp1 tabl 
                                code3 <- transStatements exp2 tabl 
                                return (code1 ++ [LABEL l1] ++ code2 ++ 
                                        [JUMP l3,LABEL l2] ++ code3 ++ 
                                        [LABEL l3])
                (While cond exp1) 
                          -> do l1 <- newLabel
                                l2 <- newLabel
                                l3 <- newLabel
                                code1 <- transCondition cond tabl l2 l3
                                code2 <- transStatements exp1 tabl 
                                return ([LABEL l1] ++ code1 ++ 
                                        [LABEL l2] ++ code2 ++ 
                                        [JUMP l1,LABEL l3])

transCondition :: Expr -> Table -> Label -> Label -> State Count [Instr]
transCondition (condition) tabl ltrue lfalse = case condition of 
               (Number 0) -> do return [JUMP lfalse ]
               (Number 1) -> do return [JUMP ltrue ]
               (Op And exp1 exp2) 
                          -> do l1 <- newLabel
                                code1 <- transCondition exp1 tabl l1 lfalse
                                code2 <- transCondition exp2 tabl ltrue lfalse
                                return (code1++[LABEL l1]++code2)
               (Op Or exp1 exp2) 
                          -> do l1 <- newLabel
                                code1 <- transCondition exp1 tabl ltrue l1
                                code2 <- transCondition exp2 tabl ltrue lfalse
                                return (code1++[LABEL l1]++code2)
               (Op op exp1 exp2) 
                          -> do t1 <- newTemp 
                                t2 <- newTemp 
                                code1 <- transExpression exp1 tabl t1
                                code2 <- transExpression exp2 tabl t2
                                popTemp 2
                                return (code1  ++ code2 ++ [COND t1 op t2 ltrue lfalse])
               (exp1) -> do t1 <- newTemp 
                            code1 <- transExpression exp1 tabl  t1
                            return (code1 ++  [COND t1 NotEquals "0" ltrue lfalse])
 
transArguments :: [Expr] -> Table -> State Count ([Instr],[Temp])
transArguments [] tabl = return ([],[])
transArguments (exp:tail) tabl = do t1 <- newTemp 
                                    code1 <- transExpression exp tabl t1 
                                    popTemp 1
                                    (code2,tmps) <- transArguments tail tabl
                                    return (code1 ++ code2,[t1] ++ tmps)
