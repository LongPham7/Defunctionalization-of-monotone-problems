module Formatter where

import DataTypes
import HelperFunctions

dataDeclaration :: [String] -> String
dataDeclaration vars = header ++ concat (map indent vars) ++ footer
  where indent s = "  " ++ s ++ "\n"
        header = "(declare-datatypes () ((Closr\n"
        footer = "  (boolCons (boolHd Bool) (boolTl Closr))\n\
                 \  (intCons (intHd Int) (intTl Closr))\n\
                 \  (closrCons (closrHd Closr) (closrTl Closr)) )))\n"

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

-- Display a type constructor in the SMT-LIB format. 
showSMTConstructor :: Sort -> String
showSMTConstructor IntSort = "intCons"
showSMTConstructor BoolSort = "boolCons"
showSMTConstructor ClosrSort = "closrCons"
showSMTConstructor _ = error "Only base sorts are accepted."

-- Display a type/sort in the SMT-LIB format. 
showSMTBaseSort :: Sort -> String
showSMTBaseSort IntSort = "Int"
showSMTBaseSort BoolSort = "Bool"
showSMTBaseSort ClosrSort = "Closr"
showSMTBaseSort _ = error "Only base sorts are accepted."