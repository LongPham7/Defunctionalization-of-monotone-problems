module Defunctionalize where

import DataTypes
import Control.Monad.State
import System.Random

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
      let t3 = AppSort (Var "IOMatch_ClosrSort") (Var varX) (Arrow ClosrSort BoolSort)
          t4 = AppSort t3 (Var varY) BoolSort
          t5 = Exists varY ClosrSort (And t2 t4)
      return (Exists varX ClosrSort (And t1 t5))
  | otherwise = do
      (x:xs) <- get
      put xs
      let varX = "x_" ++ x
      t1 <- defunctionalizeArrow u varX
      t2 <- defunctionalizeBase v
      let t3 = AppSort (Var ("IOMatch" ++ show sort)) (Var varX) (Arrow sort BoolSort)
          t4 = AppSort t3 t2 BoolSort
      return (Exists varX ClosrSort (And t1 t4))
  where sort = calculateSort v
                           
-- Defunctionalization of terms with arrow sorts

defunctionalizeArrow :: Term -> String -> State [String] Term
defunctionalizeArrow t@(VarSort v s) h
  | isHigherOrderSort s = return (Eq (Var h) t)
  | otherwise = error $ "The type of " ++ show t ++ " is first order." 
defunctionalizeArrow t@(TopVarSort v s) h
  | isHigherOrderSort s = return (Eq (Var h) (Const ("C^0_" ++ v)))
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
      let t3 = AppSort (Const "Apply_ClosrSort") (Var varX) (Arrow ClosrSort (Arrow ClosrSort BoolSort))
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
      -- App should be replaced with AppSort
      let t3 = AppSort (Const ("Apply_" ++ show s)) (Var varX) (Arrow ClosrSort (Arrow ClosrSort BoolSort))
          t4 = AppSort t3 t2 (Arrow ClosrSort BoolSort)
          t5 = AppSort t4 (Var h) BoolSort
      return (Exists varX ClosrSort (And t1 t5))
  where s = calculateSort v
defunctionalizeArrowApp u v _ _ = error "Type mismatch: higher-order sort is expected."

-- Testing

sample = AppSort (VarSort "f" (Arrow IntSort IntSort)) (Add (Num 1) (Num 3)) IntSort

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = as : (splitEvery n bs)
  where (as, bs) = splitAt n xs

sampleCs = splitEvery 3 (randomRs ('a', 'z') (mkStdGen 11) :: String)

output = fst $ runState (defunctionalizeBase sample) sampleCs