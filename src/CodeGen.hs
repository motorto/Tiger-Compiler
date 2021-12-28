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

transExp :: Table -> Expr -> Identifier -> State Count [Instr]

transExp tabl (Number n) dest
  = return [MOVEI dest n]

transExp tabl (Var (VarName x)) dest
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
       return (code1 ++ code2 ++ [OP op dest place1 place2])

transExp tabl (FuncCall id exps) dest
  = do (code1, temps) <- transExps tabl exps
       return (code1 ++ [CALL dest id temps])

------ ???
--transExp tabl (1) dest = return [MOVEI dest 1]
--transExp tabl (0) dest = return [MOVEI dest 0]
------

transExps::Table -> [Expr] -> State Count ([Instr],[Temp])
transExps tabl [] = return ([],[])
transExps tabl (expr:exprs) = do temp1 <- newTemp
                                 code1 <- transExp tabl expr temp1
                                 (code2,temps) <- transExps tabl exprs
                                 return (code1 ++ code2,[temp1] ++ temps)

transStat :: Table -> Expr -> State Count [Instr]
transStat tabl (IfThen cond stat1)
  = do label1 <- newLabel 
       label2 <- newLabel 
       code1 <- transCond tabl cond label1 label2
       code2 <- transStat tabl stat1
       return (code1 ++ [LABEL label1] ++
                code2 ++ [LABEL label2])

transStat tabl (IfThenElse cond stat1 stat2)
   = do label1 <- newLabel 
        label2 <- newLabel 
        label3 <- newLabel 
        code1 <- transCond tabl cond label1 label2
        code2 <- transStat tabl stat1
        code3 <- transStat tabl stat2 
        return (code1 ++ [LABEL label1] ++
                code2 ++ [JUMP label3 , LABEL label2] ++ 
                code3 ++ [LABEL label3])

transStat tabl (While cond stat1)
   = do label1 <- newLabel  --true
        label2 <- newLabel  -- loop
        label3 <- newLabel  -- false
        code1 <- transCond tabl cond label2 label3
        code2 <- transStat tabl stat1
        return ([LABEL label1] ++ code1 ++
                [LABEL label2] ++ code2 ++ 
                [JUMP label1, LABEL label3])

transStat tabl (Assign (VarName id) exp1) 
   = case Map.lookup id tabl of
          Nothing -> error "undefined variable"
          Just dest -> do temp1 <- newTemp 
                          code1 <- transExp tabl exp1 temp1
                          return (code1 ++ [MOVE dest temp1])

transStats :: Table -> [Expr] -> State Count [Instr]
transStats tabl [] = return []
transStats tabl (head:tail) = do code1 <- transStat tabl head
                                 codes <- transStats tabl tail
                                 return (code1 ++ codes)
                                 
transCond :: Table -> Expr -> Label -> Label -> State Count [Instr]

-- --- ???
-- transCond tabl (1) labelt labelf = return [JUMP labelt]
-- transCond tabl (0) labelt labelf = return [JUMP labelf] 
-- --- ???

transCond tabl (Cond And cond1 cond2) ltrue lfalse
    = do arg2 <- newLabel
         code1 <- transCond tabl cond1 arg2 lfalse
         code2 <- transCond tabl cond2 ltrue lfalse
         return (code1 ++ [LABEL arg2] ++ code2)

transCond tabl (Cond Or cond1 cond2) ltrue lfalse
    = do arg2 <- newLabel
         code1 <- transCond tabl cond1 ltrue arg2
         code2 <- transCond tabl cond2 ltrue lfalse
         return (code1 ++ [LABEL arg2] ++ code2)

transCond tabl (Cond op exp1 exp2) ltrue lfalse
    = do t1 <- newTemp
         t2 <- newTemp
         code1 <- transExp tabl exp1 t1
         code2 <- transExp tabl exp2 t2
         return (code1 ++ code2 ++ 
                [COND t1 op t2 ltrue lfalse])

--- ???
transCond tabl exp1 ltrue lfalse 
    = do t1 <- newTemp 
         code1 <- transExp tabl exp1 t1
         return (code1 ++  [COND t1 Equals 1 ltrue lfalse]) -- ???????? COND t != 0 
--- ???

{-
   break { Break } []
To handle these, the translation function for
statements must have an extra inherited parameter which is the label that a break
or exit statement must jump to. This attribute is changed whenever a new loop is
entered. Before the first loop is entered, this attribute is undefined.
-}
