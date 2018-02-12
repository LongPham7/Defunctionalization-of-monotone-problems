module Preproces where

import DataTypes
import Control.Monad.State
import System.Random

-- Type annotation

annotateType :: Term -> Env -> Env -> (Term, Sort)
annotateType (Var v) sortEnv env
 | v `elem` (map fst sortEnv) = let s = (typeOf v sortEnv) in (TopVarSort v s, s)
 | v `elem` (map fst env) = let s = (typeOf v env) in (VarSort v s, s)
 | otherwise = error $ "The sort environments do not contain the variable " ++ v ++ "."
annotateType t@(Num n) sortEnv env = (t, IntSort)
annotateType (Add u v) sortEnv env = annotateTypeArithmeticExp u v Add sortEnv env 
annotateType (Sub u v) sortEnv env = annotateTypeArithmeticExp u v Sub sortEnv env 
annotateType (Sma u v) sortEnv env = annotateTypeFirstOrderFormula u v Sma sortEnv env 
annotateType (SmaEq u v) sortEnv env = annotateTypeFirstOrderFormula u v SmaEq sortEnv env 
annotateType (Eq u v) sortEnv env = annotateTypeFirstOrderFormula u v Eq sortEnv env 
annotateType (Lar u v) sortEnv env = annotateTypeFirstOrderFormula u v Lar sortEnv env 
annotateType (LarEq u v) sortEnv env = annotateTypeFirstOrderFormula u v LarEq sortEnv env 
annotateType (And u v) sortEnv env = annotateTypeBoolFormula u v And sortEnv env 
annotateType (Or u v) sortEnv env = annotateTypeBoolFormula u v Or sortEnv env 
annotateType (App u v) sortEnv env = annotateTypeApp u v sortEnv env
annotateType (Exists v s b) sortEnv env = annotateTypeExists v s b sortEnv env
annotateType (Lambda v s b) sortEnv env = annotateTypeLambda v s b sortEnv env

annotateTypeBinaryOp :: (Sort, Sort) -> Term -> Term -> (Term -> Term -> Term) -> Env -> Env -> (Term, Sort)
annotateTypeBinaryOp (s1, s2) u v f sortEnv env
  | s3 == s1 && s4 == s1 = (f t1 t2, s2)
  | otherwise = error $ "Type mismatch in " ++ show (f u v)
  where (t1, s3) = annotateType u sortEnv env
        (t2, s4) = annotateType v sortEnv env

annotateTypeArithmeticExp :: Term -> Term -> (Term -> Term -> Term) -> Env -> Env -> (Term, Sort)
annotateTypeArithmeticExp = annotateTypeBinaryOp (IntSort, IntSort)

annotateTypeFirstOrderFormula :: Term -> Term -> (Term -> Term -> Term) -> Env -> Env -> (Term, Sort)
annotateTypeFirstOrderFormula = annotateTypeBinaryOp (IntSort, BoolSort)

annotateTypeBoolFormula :: Term -> Term -> (Term -> Term -> Term) -> Env -> Env -> (Term, Sort)
annotateTypeBoolFormula = annotateTypeBinaryOp (BoolSort, BoolSort)

annotateTypeExists :: String -> Sort -> Term -> Env -> Env -> (Term, Sort)
annotateTypeExists v s1 b sortEnv env
  | s2 == BoolSort = (Exists v s1 t, BoolSort)
  | otherwise = error $ "Type mismatch in " ++ show (Exists v s1 b)
  where (t, s2) = annotateType b sortEnv ((v, s1): env)

annotateTypeApp :: Term -> Term -> Env -> Env -> (Term, Sort)
annotateTypeApp u v sortEnv env 
  | s2 == source = (AppSort t1 t2 target, target)
  | otherwise = error $ "Type mismatch in " ++ show (App u v)
  where (t1, s1) = annotateType u sortEnv env
        (t2, s2) = annotateType v sortEnv env
        (source, target) = sourceTargetType s1

annotateTypeLambda :: String -> Sort -> Term -> Env -> Env -> (Term, Sort)
annotateTypeLambda v s1 b sortEnv env = (LambdaSort v s1 t s2, Arrow s1 s2)
  where (t, s2) = annotateType b sortEnv  ((v, s1): env)

-- Eta expansion

etaExpansion :: Sort -> Term -> State [String] Term
etaExpansion IntSort t = return t
etaExpansion BoolSort t = return t
etaExpansion (Arrow source target) (LambdaSort v s1 b s2)
  | s1 == source = (etaExpansion target b) >>= (\t -> return (LambdaSort v s1 t s2))
  | otherwise = error "The sort of a quantified variable does not match the source sort."
etaExpansion s t = do
  xs <- get
  let 
    sorts = init (decomposeSort s)
    (as, bs) = splitAt (length sorts) xs
    vars = map (\s -> "x_" ++ s) as
    apps = addApps t vars
    lambdas = addLambdas apps (zip vars sorts)
  put bs
  return lambdas

-- Eliminate anonymous functions

elimAnonymIgnoreLambdas :: Term -> State [String] (Term, [Equation])
elimAnonymIgnoreLambdas (LambdaSort v s1 b s2) = elimAnonymIgnoreLambdas b >>= (\(u, eqs) -> return (LambdaSort v s1 u s2, eqs))
elimAnonymIgnoreLambdas t = elimAnonym t

elimAnonym :: Term -> State [String] (Term, [Equation])
elimAnonym (Exists v s b) = elimAnonym b >>= (\(u, eqs) -> return (Exists v s u, eqs))
elimAnonym t1@(LambdaSort v s1 b s2) = do
  (t2, bs) <- elimAnonymIgnoreLambdas t1
  (x:xs) <- get
  let freeVars = extractFreeVars t1
      newVar = "X_" ++ x
      apps = addApps (Var newVar) (map fst freeVars)
      lambdas = addLambdas t2 freeVars
  put xs
  return (apps, (newVar, lambdas): bs)
elimAnonym (And u v) = do
  (t1, as) <- elimAnonym u
  (t2, bs) <- elimAnonym v
  return (And t1 t2, as ++ bs)
elimAnonym (Or u v) = do
  (t1, as) <- elimAnonym u
  (t2, bs) <- elimAnonym v
  return (Or t1 t2, as ++ bs)
elimAnonym (AppSort u v s) = do
  (t1, as) <- elimAnonym u
  (t2, bs) <- elimAnonym v
  return (AppSort t1 t2 s, as ++ bs)
elimAnonym t = return (t, [])
  
extractFreeVars :: Term -> Env
extractFreeVars (TopVarSort v s) = []
extractFreeVars (VarSort v s) = [(v, s)]
extractFreeVars (Num n) = []
extractFreeVars (Add u v) = extractFreeVars u ++ (extractFreeVars v)
extractFreeVars (Sub u v) = extractFreeVars u ++ (extractFreeVars v)
extractFreeVars (Sma u v) = extractFreeVars u ++ (extractFreeVars v)
extractFreeVars (SmaEq u v) = extractFreeVars u ++ (extractFreeVars v)
extractFreeVars (Eq u v) = extractFreeVars u ++ (extractFreeVars v)
extractFreeVars (Lar u v) = extractFreeVars u ++ (extractFreeVars v)
extractFreeVars (LarEq u v) = extractFreeVars u ++ (extractFreeVars v)
extractFreeVars (And u v)= extractFreeVars u ++ (extractFreeVars v)
extractFreeVars (Or u v)= extractFreeVars u ++ (extractFreeVars v)
extractFreeVars (AppSort u v s)= extractFreeVars u ++ (extractFreeVars v)
extractFreeVars (Exists v s b) = extractFreeVars b
extractFreeVars (LambdaSort v s1 b s2) = extractFreeVars b

-- Helper functions

sourceTargetType :: Sort -> (Sort, Sort)
sourceTargetType (Arrow s t) = (s, t)
sourceTargetType _ = error "This is not an arrow type."

typeOf :: String -> Env -> Sort
typeOf v env = head [s | (v,s) <- env]

addApps :: Term -> [String] -> Term
addApps = foldl addApp

addApp :: Term -> String -> Term
addApp t v = AppSort t (Var v) target
  where (source, target) = sourceTargetType (calculateSort t)

addLambdas :: Term -> Env -> Term
addLambdas = foldr addLambda

addLambda :: (String, Sort) -> Term -> Term
addLambda (v, s) t = LambdaSort v s t sort
  where sort = Arrow s (calculateSort t)

-- Testing

sample = Lambda "x" IntSort (App (Var "f") (Lambda "y" BoolSort (Num 1)))
sortEnv = [("f", Arrow (Arrow BoolSort IntSort) (Arrow BoolSort IntSort))]
sampleSort = Arrow IntSort (Arrow BoolSort IntSort)

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = as : (splitEvery n bs)
  where (as, bs) = splitAt n xs

sampleCs = splitEvery 3 (randomRs ('a', 'z') (mkStdGen 11) :: String)
annotatedSample = fst $ annotateType sample sortEnv []
output1 = fst $ runState (etaExpansion sampleSort annotatedSample) sampleCs
output2 = fst $ runState (elimAnonymIgnoreLambdas annotatedSample) sampleCs