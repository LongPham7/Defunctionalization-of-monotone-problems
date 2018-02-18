module Main where

import Tokeniser(alexScanTokens)
import Parser(parse)
import DataTypes
import HelperFunctions
import Preprocess
import Defunctionalize
import Control.Monad.State

main :: IO()
main = do
    s <- getContents
    let 
      tokens = alexScanTokens s
      input = parse tokens
      output = fst $ runState (produceOutput input) freshVars
    print output

-- Combine preprocessing and defunctionalization

produceOutput :: MonoProblem -> State [String] MonoProblem
produceOutput (MProblem sortEnv eqs goal) = do
  (eqs', goal') <- preprocess eqs goal sortEnv
  let applys = recursiveCreateApplys eqs'
  iomatches <- recursiveDefunctionalize eqs'
  finalGoal <- defunctionalizeBase goal'
  let finalEqs = iomatches ++ applys
      finalEnvVars = map (\(var, term) -> var) finalEqs
      finalSortEnv  = [(v, s) | (v, s) <- candidateSortEnv, v `elem` finalEnvVars]
  return (MProblem finalSortEnv finalEqs finalGoal)

candidateSortEnv :: Env
candidateSortEnv = iomatchEnv ++ applyEnv
  where iomatchEnv = [("IOMatch_" ++ show s, iomatchSort s) | s <- [IntSort, BoolSort, ClosrSort]]
        applyEnv = [("Apply_" ++ show s, applySort s) | s <- [IntSort, BoolSort, ClosrSort]]

iomatchSort :: Sort -> Sort
iomatchSort s = Arrow ClosrSort (Arrow s BoolSort)

applySort :: Sort -> Sort
applySort s = Arrow ClosrSort (Arrow s (Arrow ClosrSort BoolSort))

-- Preprocessing

preprocess :: [Equation] -> Term -> Env -> State [String] ([Equation], Term)
preprocess eqs goal sortEnv = do
  let annotatedEqs = recursiveAnnotateType eqs sortEnv
      annotatedGoal = fst $ annotateType goal sortEnv []
  eqs2 <- recursiveElimAnonym annotatedEqs
  (goal', eqs3) <- elimAnonym annotatedGoal
  eqs4 <- recursiveEtaExpansion (eqs3 ++ eqs2)
  return (eqs4, goal')

recursivePreprocess :: [Equation] -> Env -> State [String] [Equation]
recursivePreprocess eqs1 sortEnv = do
  let typeAnnotated = recursiveAnnotateType eqs1 sortEnv
  eqs2 <- recursiveElimAnonym typeAnnotated
  eqs3 <- recursiveEtaExpansion eqs2
  return eqs3

recursiveAnnotateType :: [Equation] -> Env -> [Equation]
recursiveAnnotateType eqs sortEnv = map annotate eqs
  where annotate (var, term) = (var, fst $ annotateType term sortEnv [])

recursiveElimAnonym :: [Equation] -> State [String] [Equation]
recursiveElimAnonym [] = return []
recursiveElimAnonym ((var, term): eqs) = do
  (term', eqs1) <- elimAnonymIgnoreLambdas term
  eqs2 <- recursiveElimAnonym eqs
  return ((var, term'): (eqs1 ++ eqs2))

recursiveEtaExpansion :: [Equation] -> State [String] [Equation]
recursiveEtaExpansion [] = return []
recursiveEtaExpansion ((var, term): eqs) = do
  term' <- etaExpansion (calculateSort term) term
  eqs' <- recursiveEtaExpansion eqs
  return ((var, term'): eqs')

-- Defunctionalization

recursiveCreateApplys :: [Equation] -> [Equation]
recursiveCreateApplys eqs = concat (map produceApplys eqs)
  where produceApplys (var, term) = createApplys var (calculateSort term)

recursiveDefunctionalize :: [Equation] -> State [String] [Equation]
recursiveDefunctionalize [] = return []
recursiveDefunctionalize ((var, term): eqs) = do
  eq' <- defunctionalize var term
  eqs' <- recursiveDefunctionalize eqs
  return (eq': eqs')