module CodeGen where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import IR
import Parser

type Table = Map Identifier String

type Count = (Int, Int) -- counter for temps and labels

newTemp :: State Count Temp
newTemp = do (t, l) <- get; put (t + 1, l); return ("t" ++ show t)

popTemp :: Int -> State Count ()
popTemp k = modify (\(t, l) -> (t - k, l))

newLabel :: State Count Label
newLabel = do (t, l) <- get; put (t, l + 1); return ("L" ++ show l)

transExpression :: Expr -> Table -> Identifier -> Identifier -> State Count [Instr]
transExpression expression tabl dest breakLabel = case expression of
  (Number n) -> return [MOVEI dest n]
  (BuildString str) -> return [MOVES dest str]
  (Var (VarName x)) -> case Map.lookup x tabl of
    Just temp -> return [MOVE dest temp]
    Nothing -> error "invalid variable"
  (Op op exp1 exp2) ->
    do
      t1 <- newTemp
      t2 <- newTemp
      code1 <- transExpression exp1 tabl t1 breakLabel
      code2 <- transExpression exp2 tabl t2 breakLabel
      popTemp 2
      return (code1 ++ code2 ++ [OP op dest t1 t2])
  (Negative exp1) ->
    do
      t1 <- newTemp
      code1 <- transExpression exp1 tabl t1 breakLabel
      popTemp 1
      return (code1 ++ [OP Subtraction t1 "0" t1])
  (FuncCall id args) ->
    do
      (code, temps) <- transArguments args tabl
      popTemp (length args)
      return (code ++ [CALL dest id temps])
  (ExpSeq args) ->
    do
      (code, temps) <- transArguments args tabl
      popTemp (length args)
      return code
  (IfThen cond exp1) ->
    do
      l1 <- newLabel
      l2 <- newLabel
      code1 <- transCondition cond tabl l1 l2
      t1 <- newTemp
      code2 <- transExpression exp1 tabl t1 breakLabel
      popTemp 1
      return
        ( code1 ++ [LABEL l1]
            ++ code2
            ++ [LABEL l2]
        )
  (IfThenElse cond exp1 exp2) ->
    do
      l1 <- newLabel
      l2 <- newLabel
      l3 <- newLabel
      code1 <- transCondition cond tabl l1 l2
      t1 <- newTemp
      code2 <- transExpression exp1 tabl t1 breakLabel
      t2 <- newTemp
      code3 <- transExpression exp2 tabl t2 breakLabel
      popTemp 2
      return
        ( code1 ++ [LABEL l1] ++ code2
            ++ [JUMP l3, LABEL l2]
            ++ code3
            ++ [LABEL l3]
        )
  (While cond exp1) ->
    do
      l1 <- newLabel
      l2 <- newLabel
      l3 <- newLabel
      code1 <- transCondition cond tabl l2 l3
      t1 <- newTemp
      code2 <- transExpression exp1 tabl t1 l3
      popTemp 1
      return
        ( [LABEL l1] ++ code1
            ++ [LABEL l2]
            ++ code2
            ++ [JUMP l1, LABEL l3]
        )
  (Assign (VarName x) expr) ->
    case Map.lookup x tabl of
      Nothing -> error "undefined variable"
      Just dest -> do
        t1 <- newTemp
        code1 <- transExpression expr tabl t1 breakLabel
        popTemp 1
        return (code1 ++ [MOVE dest t1])
  (LetIn vars exp1) ->
    do
      (code1, newTable) <- transVarDecls vars tabl
      (code2, tmps) <- transArguments exp1 newTable
      popTemp (length tmps)
      return (code1 ++ code2)
  Break ->
    do return [JUMP breakLabel]

transArguments :: [Expr] -> Table -> State Count ([Instr], [Temp])
transArguments [] tabl = return ([], [])
transArguments (exp : tail) tabl = do
  t1 <- newTemp
  code1 <- transExpression exp tabl t1 ""
  (code2, tmps) <- transArguments tail tabl
  return (code1 ++ code2, t1 : tmps)

transVarDecl :: VarDecl -> Table -> State Count ([Instr], Table)
transVarDecl varDecl tabl = case varDecl of
  (Decl id exp1) ->
    do
      t1 <- newTemp
      code <- transExpression exp1 tabl t1 ""
      return (code, Map.insert id t1 tabl)

transVarDecls :: [VarDecl] -> Table -> State Count ([Instr], Table)
transVarDecls [] tabl = return ([], tabl)
transVarDecls (v : vs) tabl = do
  (code1, tabl1) <- transVarDecl v tabl
  (code2, tabl2) <- transVarDecls vs tabl1
  return (code1 ++ code2, tabl2)

transCondition :: Expr -> Table -> Label -> Label -> State Count [Instr]
transCondition condition tabl ltrue lfalse = case condition of
  (Number 0) -> do return [JUMP lfalse]
  (Number n) -> do return [JUMP ltrue]
  (Op And exp1 exp2) ->
    do
      l1 <- newLabel
      code1 <- transCondition exp1 tabl l1 lfalse
      code2 <- transCondition exp2 tabl ltrue lfalse
      return (code1 ++ [LABEL l1] ++ code2)
  (Op Or exp1 exp2) ->
    do
      l1 <- newLabel
      code1 <- transCondition exp1 tabl ltrue l1
      code2 <- transCondition exp2 tabl ltrue lfalse
      return (code1 ++ [LABEL l1] ++ code2)
  (Op op exp1 exp2) ->
    do
      t1 <- newTemp
      t2 <- newTemp
      code1 <- transExpression exp1 tabl t1 ""
      code2 <- transExpression exp2 tabl t2 ""
      return (code1 ++ code2 ++ [COND t1 op t2 ltrue lfalse])
  exp1 -> do
    t1 <- newTemp
    code1 <- transExpression exp1 tabl t1 ""
    return (code1 ++ [COND t1 NotEquals "0" ltrue lfalse])

transType :: TypeField -> Table -> State Count ([Temp], Table)
transType typeField tabl = case typeField of
  (Declare id typeId) ->
    do
      t1 <- newTemp
      return ([t1], Map.insert id t1 tabl)

transTypes :: [TypeField] -> Table -> State Count ([Temp], Table)
transTypes [] tabl = return ([], tabl)
transTypes (t : ts) tabl = do
  (t1, tabl1) <- transType t tabl
  (t2, tabl2) <- transTypes ts tabl1
  return (t1 ++ t2, tabl2)

transDeclaration :: Decl -> Table -> State Count ([Instr], Table)
transDeclaration declaration tabl = case declaration of
  (VarDeclaration (Decl id exp1)) ->
    do
      t1 <- newTemp
      code <- transExpression exp1 tabl t1 ""
      return (code, Map.insert id t1 tabl)
  (FunDeclaration (FunctionDeclare id args exp1)) ->
    do
      let table1 = Map.insert id id tabl
      (tmps, table2) <- transTypes args table1
      t1 <- newTemp
      code1 <- transExpression exp1 table2 t1 ""
      return ([FUN id tmps code1], table1)
  (FunDeclaration (FunctionDeclareTyped id args typ exp1)) ->
    do
      let table1 = Map.insert id id tabl
      (tmps, table2) <- transTypes args table1
      t1 <- newTemp
      code1 <- transExpression exp1 table2 t1 ""
      return ([FUN id tmps code1], table2)

transDeclarations :: [Decl] -> Table -> State Count ([Instr], Table)
transDeclarations [] tabl = return ([], tabl)
transDeclarations (dec : decs) tabl = do
  (code1, tabl1) <- transDeclaration dec tabl
  (code2, tabl2) <- transDeclarations decs tabl1
  return (code1 ++ code2, tabl2)

transProgram :: Program -> State Count [Instr]
transProgram (Begin decls exprs) =
  do
    let tabl = Map.fromList [("print", "print"), ("printi", "printi"), ("scani", "scani")]
    (code1, table1) <- transDeclarations decls tabl
    (code2, tmps) <- transArguments exprs table1
    return (code2 ++ code1)
