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

-- Definitions of top-level relational variables in the extended SMT-LIB2 format

defineApplyExt :: (String, Sort) -> [Term] -> State [String] String
defineApplyExt (var, sort) terms = do
  (x:y:z:zs) <- get
  put zs
  let varX = "x_" ++ x
      varY = "x_" ++ y
      varZ = "x_" ++ z
      header = "(declare-rel " ++ var ++ " "
      sorts = init $ decomposeSort sort
      domainSorts = "(" ++ (unwords $ map showSMTBaseSort sorts) ++ "))"
      terms' = map (renameApply (varX, varY, varZ)) terms
      env = zip [varX, varY, varZ] sorts
      rules = map (\t -> defineRuleExt t var env) terms'
      varDeclarations = map declareVariable (zip [varX, varY, varZ] sorts)
      result = unlines (((header ++ domainSorts): varDeclarations) ++ rules)
  return result

defineIOMatchExt :: (String, Sort) -> [Term] -> State [String] String
defineIOMatchExt (var, sort) terms = do
  (x:y:ys) <- get
  put ys
  let varX = "x_" ++ x
      varY = "x_" ++ y
      sorts = init $ decomposeSort sort
      header = "(declare-rel " ++ var ++ " "
      domainSorts = "(" ++ (unwords $ map showSMTBaseSort sorts) ++ "))"
      varDeclarations = map declareVariable (zip [varX, varY] sorts)
      terms' = map (renameIOMatch (varX, varY)) terms
      env = zip [varX, varY] sorts
      rules = map (\t -> defineRuleExt t var env) terms'
      result = unlines (((header ++ domainSorts): varDeclarations) ++ rules)
  return result

declareVariable :: (String, Sort) -> String
declareVariable (v, s) = "(declare-var " ++ v ++ " " ++ showSMTBaseSort s ++ ")"

defineRuleExt :: Term -> String -> Env -> String
defineRuleExt t var env = header ++ footer
  where t' = defineBody t
        header = "(rule (=> " ++ t' ++ " "
        footer = "(" ++ unwords (var: (map fst env)) ++ ")))"

-- This alpha-converts a definition of Apply. The outermost lambda abstractions
-- are removed from the output. 
renameApply :: (String, String, String) -> Term -> Term
renameApply (first, second, third) (LambdaSort v1 s1 (LambdaSort v2 s3 (LambdaSort v3 s5 b s6) s4) s2) = result
  where result = rename b [(v1, first), (v2, second), (v3, third)]

-- This alpha-converts a definition of IOMatch. The outermost lambda
-- abstractions are removed from the output. 
renameIOMatch :: (String, String) -> Term -> Term
renameIOMatch (first, second) (LambdaSort v1 s1 (LambdaSort v2 s3 b s4) s2) = rename b [(v1, first), (v2, second)]

-- Definitions of top-level relational variables in the pure SMT-LIB2 format

-- This can be used for both Applys and IOMatches. 
defineRelationalVariablePure :: (String, Sort) -> [Term] -> String
defineRelationalVariablePure (var, sort) terms = result
  where sorts = decomposeSort sort
        header = "(declare-fun " ++ var ++ " "
        domainSorts = "(" ++ (unwords $ map showSMTBaseSort (init sorts)) ++ ") "
        targetSort = showSMTBaseSort (last sorts) ++ ")"
        terms' = map (\t -> defineAssert var t) terms
        result = unlines ((header ++ domainSorts ++ targetSort): terms')

defineAssert :: String -> Term -> String
defineAssert var term = header ++ domainSorts ++ definition
  where (term', env) = decomposeLambdas term
        header = "(assert (forall ("
        domainSorts = unwords $ map (\(v,s) -> "(" ++ v ++ " " ++ showSMTBaseSort s ++ ")") env
        definition = ") (=> " ++ defineBody term' ++ " (" ++ var ++ " " ++ (unwords $ map fst env) ++ "))))"

-- Definitions of goal terms free of lambda abstractions in the extended and
-- pure SMT-LIB2 format

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
defineBody t = error (show t ++ " cannot be translated into the SMT-LIB2.")

-- Helper functions

-- This displays a type constructor in the SMT-LIB2 format.
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
