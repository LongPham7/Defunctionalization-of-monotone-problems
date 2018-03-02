module Defunctionalize where

import DataTypes
import HelperFunctions
import Control.Monad.State

-- Produce definitions of Apply_B

createApplys :: String -> Sort -> [Equation]
createApplys var s = map (createApply var) cumulativeSorts
  where sorts = decomposeSort s
        sorts' = take (length sorts - 1) sorts
        cumulativeSorts = drop 1 $ scanl (\xs -> \x -> xs ++ [x]) [] sorts'

createApply :: String -> [Sort] -> Equation
createApply var ts = ("Apply_" ++ show lastSort, secondLambda)
  where arity = length sorts
        vars = map (\n -> "a_" ++ show n) [1..(arity - 1)]
        sorts = map defunctionalizeSort ts
        lastSort = last sorts
        cSortFirst = constSort $ init sorts
        constFirst = Const (constName var(arity - 1)) cSortFirst
        appsFirst = addApps constFirst vars
        cSortSecond = constSort sorts
        constSecond = Const (constName var arity) cSortSecond
        appsSecond = addApps constSecond (vars ++ ["y"])
        conjunction = And (Eq (VarSort "x" ClosrSort) appsFirst) (Eq (VarSort "z" ClosrSort) appsSecond)
        exists = addExists conjunction (zip vars sorts)
        firstLambda = LambdaSort "y" lastSort (LambdaSort "z" ClosrSort exists BoolSort) (Arrow ClosrSort BoolSort)
        secondLambda = LambdaSort "x" ClosrSort firstLambda (Arrow lastSort (Arrow ClosrSort BoolSort))

constSort :: [Sort] -> Sort
constSort [] = ClosrSort
constSort (t:ts) = Arrow (defunctionalizeSort t) (constSort ts)

constName :: String -> Int -> String
constName var n = "C^" ++ show n ++ "_" ++ var

-- Produce definitions of IOMatch_B

defunctionalize :: String -> Term -> State [String] Equation
defunctionalize var t = do
  (x:xs) <- get
  put xs
  let varX = "x_" ++ x
      (b, vars) = decomposeLambdas t
      vars' = init vars
      (x_m, s) = last vars
      arity = length vars
  b' <- defunctionalizeBase b
  let cSort = constSort $ map snd vars'
      const = Const (constName var (arity - 1)) cSort
      apps = addApps const (map fst vars')
      conjunction = And (Eq (Var varX) apps) b'
      exists = addExists conjunction vars'
      lambdas = LambdaSort varX ClosrSort (LambdaSort x_m s exists BoolSort) (Arrow s BoolSort)
  return ("IOMatch_" ++ show s, lambdas)

-- Defunctionalization of terms with base sorts

defunctionalizeBase :: Term -> State [String] Term
defunctionalizeBase t@(Num n) = return t
defunctionalizeBase t@(VarSort v s)
  | isHigherOrderSort s = error $ "The type of " ++ show t ++ " is higher order." 
  | otherwise = return t
defunctionalizeBase t@(Add u v) = return t
defunctionalizeBase t@(Sub u v) = return t
defunctionalizeBase t@(Sma u v) = return t
defunctionalizeBase t@(SmaEq u v) = return t
defunctionalizeBase t@(Eq u v) = return t
defunctionalizeBase t@(Lar u v) = return t
defunctionalizeBase t@(LarEq u v) = return t
defunctionalizeBase (And u v) = do
  t1 <- defunctionalizeBase u
  t2 <- defunctionalizeBase v
  return (And t1 t2)
defunctionalizeBase (Or u v) = do
  t1 <- defunctionalizeBase u
  t2 <- defunctionalizeBase v
  return (Or t1 t2)
defunctionalizeBase (AppSort u v s) = defunctionalizeBaseApp u v s
defunctionalizeBase (Exists v s b) = defunctionalizeBase b >>= (\u -> return (Exists v s u))

defunctionalizeBaseApp :: Term -> Term -> Sort -> State [String] Term
defunctionalizeBaseApp u v s@(Arrow s1 s2) = error $ "The type of " ++ show (AppSort u v s) ++ " is higher order."
defunctionalizeBaseApp u v _
  | isHigherOrderSort sort = do
      (x:y:ys) <- get
      put ys
      let varX = "x_" ++ x
          varY = "x_" ++ y
      t1 <- defunctionalizeArrow u varX
      t2 <- defunctionalizeArrow v varY
      let t3 = AppSort (TopVarSort ("IOMatch_" ++ show ClosrSort) sortIOMatch) (Var varX) (Arrow ClosrSort BoolSort)
          t4 = AppSort t3 (Var varY) BoolSort
          t5 = Exists varY ClosrSort (And t2 t4)
      return (Exists varX ClosrSort (And t1 t5))
  | otherwise = do
      (x:xs) <- get
      put xs
      let varX = "x_" ++ x
      t1 <- defunctionalizeArrow u varX
      t2 <- defunctionalizeBase v
      let t3 = AppSort (TopVarSort ("IOMatch_" ++ show sort) sortIOMatch) (Var varX) (Arrow sort BoolSort)
          t4 = AppSort t3 t2 BoolSort
      return (Exists varX ClosrSort (And t1 t4))
  where sort = calculateSort v
        sortIOMatch = Arrow ClosrSort (Arrow (defunctionalizeSort sort) BoolSort)
                           
-- Defunctionalization of terms with arrow sorts

defunctionalizeArrow :: Term -> String -> State [String] Term
defunctionalizeArrow t@(VarSort v s) h
  | isHigherOrderSort s = return (Eq (VarSort h ClosrSort) (VarSort v ClosrSort))
  | otherwise = error $ "The type of " ++ show t ++ " is first order." 
defunctionalizeArrow t@(TopVarSort v s) h
  | isHigherOrderSort s = return (Eq (VarSort h ClosrSort) (Const ("C^0_" ++ v) ClosrSort))
  | otherwise = error $ "The top-level relational variable " ++ show t ++ "has a base sort."
defunctionalizeArrow (AppSort u v s) h = defunctionalizeArrowApp u v s h

defunctionalizeArrowApp :: Term -> Term -> Sort -> String -> State [String] Term
defunctionalizeArrowApp u v (Arrow s1 s2) h
  | isHigherOrderSort s = do
      (x:y:ys) <- get
      put ys
      let varX = "x_" ++ x
          varY = "x_" ++ y
      t1 <- defunctionalizeArrow u varX
      t2 <- defunctionalizeArrow v varY
      let t3 = AppSort (TopVarSort ("Apply_" ++ show ClosrSort) sortApply) (Var varX) (Arrow ClosrSort (Arrow ClosrSort BoolSort))
          t4 = AppSort t3 (Var varY) (Arrow ClosrSort BoolSort)
          t5 = AppSort t4 (Var h) BoolSort
          t6 = Exists varY ClosrSort (And t2 t5)
      return (Exists varX ClosrSort (And t1 t6))
  | otherwise = do
      (x:xs) <- get
      put xs
      let varX = "x_" ++ x
      t1 <- defunctionalizeArrow u varX
      t2 <- defunctionalizeBase v
      let t3 = AppSort (TopVarSort ("Apply_" ++ show s) sortApply) (Var varX) (Arrow ClosrSort (Arrow ClosrSort BoolSort))
          t4 = AppSort t3 t2 (Arrow ClosrSort BoolSort)
          t5 = AppSort t4 (Var h) BoolSort
      return (Exists varX ClosrSort (And t1 t5))
  where s = calculateSort v
        sortApply = Arrow ClosrSort (Arrow (defunctionalizeSort s) (Arrow ClosrSort ClosrSort))
defunctionalizeArrowApp u v s _ = error $ "The type of " ++ show (AppSort u v s) ++ " is first order."

-- Testing

fSort = Arrow IntSort (Arrow IntSort BoolSort)
body = AppSort (AppSort (VarSort "f" fSort) (VarSort "x" IntSort) (Arrow IntSort BoolSort)) (VarSort "y" IntSort) BoolSort
firstLambda = LambdaSort "y" IntSort body BoolSort
secondLambda = LambdaSort "x" IntSort firstLambda (Arrow IntSort BoolSort)
sample = LambdaSort "f" fSort secondLambda (Arrow IntSort (Arrow IntSort BoolSort))

output = fst $ runState (defunctionalize "Apply" sample) freshVars