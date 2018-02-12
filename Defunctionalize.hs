module Defunctionalize where

import DataTypes
import Control.Monad.State
import System.Random

-- Defunctionalization of terms with base sorts

defunctionalizeBase :: Term -> State [String] Term
defunctionalizeBase t@(Num n) = return t
defunctionalizeBase t@(VarSort v s)
  | isHigherOrderSort s = error "The type is higher order." 
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
defunctionalizeBase (Exists v s b) = defunctionalizeBase b >>= (\u -> return (Exists v s b))

defunctionalizeBaseApp :: Term -> Term -> Sort -> State [String] Term
defunctionalizeBaseApp u v (Arrow s1 s2) = error "Type mismatch: first-order sort is expected."
defunctionalizeBaseApp u v _
  | isHigherOrderSort s = do
      (x:y:ys) <- get
      put ys
      let newVarX = "x_" ++ x
          newVarY = "x_" ++ y
      t1 <- defunctionalizeArrow u newVarX
      t2 <- defunctionalizeArrow v newVarY
      -- App should be replaced with AppSort
      let t3 = App (App (Var "IOMatch_ClosrSort") (Var newVarX)) (Var newVarY)
          t4 = Exists newVarY ClosrSort (And t2 t3)
      return (Exists newVarX ClosrSort (And t1 t4))
  | otherwise = do
      (x:xs) <- get
      put xs
      let newVarX = "x_" ++ x
      t1 <- defunctionalizeArrow u newVarX
      t2 <- defunctionalizeBase v
      -- App should be replaced with AppSort
      let t3 = App (App (Var ("IOMatch" ++ show s)) (Var newVarX)) t2
      return (Exists newVarX ClosrSort (And t1 t3))
  where s = calculateSort v
                           
-- Defunctionalization of terms with arrow sorts

defunctionalizeArrow :: Term -> String -> State [String] Term
defunctionalizeArrow t@(VarSort v s) h
  | isHigherOrderSort s = return (Eq (Var h) t)
  | otherwise = error "The type is a base sort."
defunctionalizeArrow (TopVarSort v s) h
  | isHigherOrderSort s = return (Eq (Var h) (Const ("C^0_" ++ v)))
  | otherwise = error "Top-level relational variables are required to have arrow sorts."
defunctionalizeArrow (AppSort u v s) h = defunctionalizeArrowApp u v s h

defunctionalizeArrowApp :: Term -> Term -> Sort -> String -> State [String] Term
defunctionalizeArrowApp u v (Arrow s1 s2) h
  | isHigherOrderSort s = do
      (x:y:ys) <- get
      put ys
      let newVarX = "x_" ++ x
          newVarY = "x_" ++ y
      t1 <- defunctionalizeArrow u newVarX
      t2 <- defunctionalizeArrow v newVarY
      -- App should be replaced with AppSort
      let t3 = App (App (App (Const "Apply_ClosrSort") (Var newVarX)) (Var newVarY)) (Var h)
          t4 = Exists newVarY ClosrSort (And t2 t3)
      return (Exists newVarX ClosrSort (And t1 t4))
  | otherwise = do
      (x:xs) <- get
      put xs
      let newVarX = "x_" ++ x
      t1 <- defunctionalizeArrow u newVarX
      t2 <- defunctionalizeBase v
      -- App should be replaced with AppSort
      let t3 = App (App (App (Const ("Apply_" ++ show s)) (Var newVarX)) t2) (Var h)
      return (Exists newVarX ClosrSort (And t1 t3))
  where s = calculateSort v
defunctionalizeArrowApp u v _ _ = error "Type mismatch: higher-order sort is expected."

-- Testing

sample = AppSort (VarSort "f" (Arrow IntSort IntSort)) (Add (Num 1) (Num 3)) IntSort

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = as : (splitEvery n bs)
  where (as, bs) = splitAt n xs

sampleCs = splitEvery 3 (randomRs ('a', 'z') (mkStdGen 11) :: String)

output = fst $ runState (defunctionalizeBase sample) sampleCs