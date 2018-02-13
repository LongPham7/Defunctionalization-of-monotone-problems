module HelperFunctions where

import DataTypes

isHigherOrderSort :: Sort -> Bool
isHigherOrderSort s
  | s == IntSort || s == BoolSort = False
  | otherwise = True

decomposeSort :: Sort -> [Sort]
decomposeSort IntSort = [IntSort]
decomposeSort BoolSort = [BoolSort]
decomposeSort (Arrow source target) = source : decomposeSort target

calculateSort :: Term -> Sort
calculateSort (TopVarSort v s) = s
calculateSort (VarSort v s) = s
calculateSort (Const c s) = s
calculateSort (Num n) = IntSort
calculateSort (Add u v) = IntSort
calculateSort (Sub u v) = IntSort
calculateSort (Sma u v) = BoolSort
calculateSort (SmaEq u v) = BoolSort
calculateSort (Eq u v) = BoolSort
calculateSort (Lar u v) = BoolSort
calculateSort (LarEq u v) = BoolSort
calculateSort (And u v) = BoolSort
calculateSort (Or u v) = BoolSort
calculateSort (AppSort u v s) = s
calculateSort (Exists v s b) = BoolSort
calculateSort (LambdaSort v s1 b s2) = Arrow s1 s2

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
addLambda (v, s) t = LambdaSort v s t (calculateSort t)

addExists :: Term -> Env -> Term
addExists = foldr addExist
  where addExist (v, s) t = Exists v s t