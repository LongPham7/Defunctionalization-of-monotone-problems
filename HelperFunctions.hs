module HelperFunctions where

import DataTypes
import System.Random

isHigherOrderSort :: Sort -> Bool
isHigherOrderSort (Arrow s1 s2) = True
isHigherOrderSort _ = False

-- This decomposes a sort into a list of sorts.
decomposeSort :: Sort -> [Sort]
decomposeSort IntSort = [IntSort]
decomposeSort BoolSort = [BoolSort]
decomposeSort (Arrow source target) = source : (decomposeSort target)

-- This creates a composite/arrow sort from a list of sorts. This acts the
-- inverse of decomposeSort. 
concatSort :: [Sort] -> Sort
concatSort [t] = t
concatSort (t:ts) = Arrow t (concatSort ts)
concatSort _ = error "No sort is given."

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
calculateSort t = error (show t ++ " does not have type annotation.")

sourceTargetType :: Sort -> (Sort, Sort)
sourceTargetType (Arrow s t) = (s, t)
sourceTargetType s = error $ show s ++ " is not an arrow sort."

typeOf :: String -> Env -> Sort
typeOf var env = head [s | (v,s) <- env, v ==  var]

addApps :: Term -> Env -> Term
addApps = foldl addApp

addApp :: Term -> (String, Sort) -> Term
addApp t (v, s) 
  | s == source = AppSort t (VarSort v s) target
  | otherwise = error "The source type is wrong."
  where (source, target) = sourceTargetType (calculateSort t)

addLambdas :: Term -> Env -> Term
addLambdas = foldr addLambda

addLambda :: (String, Sort) -> Term -> Term
addLambda (v, s) t = LambdaSort v s t (calculateSort t)

addExists :: Term -> Env -> Term
addExists = foldr addExist
  where addExist (v, s) t = Exists v s t

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = as : (splitEvery n bs)
  where (as, bs) = splitAt n xs

decomposeLambdas :: Term -> (Term, Env)
decomposeLambdas (LambdaSort v s1 b s2) = (t, (v, defunctionalizeSort s1):env)
  where (t, env) = decomposeLambdas b
decomposeLambdas t = (t, [])

-- Renaming by a substitution

rename :: Term -> [(String, String)] -> Term
rename t@(VarSort v s) subst
  | null matches  = t
  | otherwise = VarSort (snd $ head matches) s
  where matches = [(old, new) | (old, new) <- subst, v ==  old]
rename (Add u v) subst = Add (rename u subst) (rename v subst)
rename (Sub u v) subst = Sub (rename u subst) (rename v subst)
rename (Sma u v) subst = Sma (rename u subst) (rename v subst)
rename (SmaEq u v) subst = SmaEq (rename u subst) (rename v subst)
rename (Eq u v) subst = Eq (rename u subst) (rename v subst)
rename (Lar u v) subst = Lar (rename u subst) (rename v subst)
rename (LarEq u v) subst = LarEq (rename u subst) (rename v subst)
rename (And u v) subst = And (rename u subst) (rename v subst)
rename (Or u v) subst = Or (rename u subst) (rename v subst)
rename (AppSort u v s) subst = AppSort (rename u subst) (rename v subst) s
rename (Exists v s b) subst = Exists v s (rename b subst)
rename t subst = t

-- Defunctionalization of sorts

defunctionalizeSort :: Sort -> Sort
defunctionalizeSort s
  | isHigherOrderSort s = ClosrSort
  | otherwise = s

-- Infinite seqence of random strings for fresh variables
freshVars = splitEvery 3 (randomRs ('a', 'z') (mkStdGen 11) :: String)