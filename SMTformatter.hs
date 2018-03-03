module SMTformatter where

import DataTypes
import HelperFunctions
import Control.Monad.State

-- Declaration of the algebraic data type of Closr

dataDeclaration :: [String] -> String
dataDeclaration vars = header ++ concat (map indent vars) ++ footer
  where indent s = "  " ++ s ++ "\n"
        header = "(declare-datatypes () ((Closr\n"
        footer = "  (boolCons (boolHd Bool) (boolTl Closr))\n\
                 \  (intCons (intHd Int) (intTl Closr))\n\
                 \  (closrCons (closrHd Closr) (closrTl Closr)) )))\n"

-- Definitions of first-order constructors in the form of C^i_X, where X is a 
-- top-level relational variable. 

defineSignature :: Env -> String
defineSignature env = concat (map defineConstructors env)

defineConstructors :: (String, Sort) -> String
defineConstructors (var, s) = concat (map (defineConstructor var) cumulativeSorts)
  where sorts = decomposeSort s
        requiredLength = length sorts - 2
        sorts' = take requiredLength sorts
        cumulativeSorts = map (\n -> take n sorts') [0..requiredLength]

defineConstructor :: String -> [Sort] -> String
defineConstructor var sorts = result
  where arity = length sorts
        header = "(define-fun C^" ++ show arity ++ "_" ++ var ++ " "
        sorts' = zip (map defunctionalizeSort sorts) [1..]
        showSourceSort (s,n) = "(x_" ++ show n ++ " " ++ showSMTBaseSort s ++ ")"
        domainSorts = unwords (map showSourceSort sorts')
        definition = defineClosr var sorts'
        result = header ++ "(" ++ domainSorts ++ ") Closr\n  " ++ definition ++ "\n)\n"

defineClosr :: String -> [(Sort, Int)] -> String
defineClosr var list = foldl appendClosr var list
  where appendClosr accum (s, n) = "(" ++ showSMTConstructor s ++ " x_" ++ show n ++ " " ++ accum ++ ")"

-- Definitions of top-level relational variables in the target monotone problem

defineApplys :: (String, Sort) -> [Term] -> String
defineApplys (var, sort) terms = result
  where terms' = map (fst. decomposeLambdas) terms
        definition = addOrs (map defineBody terms')
        header = "(define-fun " ++ var ++ " "
        sorts = decomposeSort sort
        showSourceSort (v, s) = "(" ++ v ++ " " ++ showSMTBaseSort s ++ ")"
        domainSorts = unwords (map showSourceSort (zip ["x", "y", "z"] sorts))
        result = header ++ "(" ++ domainSorts ++ ") Closr\n" ++ definition ++ "\n)\n" 

defineIOMatches :: (String, Sort) -> [Term] -> State [String] String
defineIOMatches (var, sort) terms = do
  (x:y:ys) <- get
  put ys
  let varX = "x_" ++ x
      varY = "x_" ++ y
      terms' = map (renameIOMatches (varX, varY)) terms
      definition = addOrs (map defineBody terms')
      header = "(define-fun " ++ var ++ " "
      sorts = decomposeSort sort
      showSourceSort (v, s) = "(" ++ v ++ " " ++ showSMTBaseSort s ++ ")"
      domainSorts = unwords (map showSourceSort (zip [varX, varY] sorts))
      result = header ++ "(" ++ domainSorts ++ ") Bool\n" ++ definition ++ "\n)\n"
  return result

renameIOMatches :: (String, String) -> Term -> Term
renameIOMatches (first, second) (LambdaSort v1 s1 (LambdaSort v2 s3 b s4) s2) = rename b [(v1, first), (v2, second)]

defineBody :: Term -> String
defineBody (TopVarSort v s) = v
defineBody (VarSort v s) = v
defineBody (Const c s) = c
defineBody (Num n) = show n
defineBody (Add u v) = "(+ " ++ defineBody u ++ " " ++ defineBody v ++ ")"
defineBody (Sub u v) = "(- " ++ defineBody u ++ " " ++ defineBody v ++ ")"
defineBody (Sma u v) = "(< " ++ defineBody u ++ " " ++ defineBody v ++ ")"
defineBody (SmaEq u v) = "(<= " ++ defineBody u ++ " " ++ defineBody v ++ ")"
defineBody (Eq u v) = "(= " ++ defineBody u ++ " " ++ defineBody v ++ ")"
defineBody (Lar u v) = "(> " ++ defineBody u ++ " " ++ defineBody v ++ ")"
defineBody (LarEq u v) = "(> " ++ defineBody u ++ " " ++ defineBody v ++ ")"
defineBody (And u v) = "(and " ++ defineBody u ++ " " ++ defineBody v ++ ")"
defineBody (Or u v) = "(or " ++ defineBody u ++ " " ++ defineBody v ++ ")"
defineBody (AppSort u v s)
  | isHigherOrderSort s = defineBody u ++ " " ++ defineBody v
  | otherwise = "(" ++ defineBody u ++ " " ++ defineBody v ++ ")"
defineBody (Exists v s b) = "(exists ((" ++ v ++ " " ++ showSMTBaseSort s ++ ")) " ++ defineBody b ++ ")"
defineBody t = error (show t ++ " cannot be translated into SMT-LIB.")

-- Helper functions

-- This displays a type constructor in the SMT-LIB format.
showSMTConstructor :: Sort -> String
showSMTConstructor IntSort = "intCons"
showSMTConstructor BoolSort = "boolCons"
showSMTConstructor ClosrSort = "closrCons"
showSMTConstructor _ = error "Only base sorts are accepted."

-- This displays a type/sort in the SMT-LIB format. 
showSMTBaseSort :: Sort -> String
showSMTBaseSort IntSort = "Int"
showSMTBaseSort BoolSort = "Bool"
showSMTBaseSort ClosrSort = "Closr"
showSMTBaseSort _ = error "Only base sorts are accepted."

addOrs :: [String] -> String
addOrs = foldl1 addOr
  where addOr str1 str2 = "(or " ++ str1 ++ "\n" ++ str2 ++ ")"
