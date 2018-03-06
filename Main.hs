module Main where

import Tokeniser(alexScanTokens)
import Parser(parse)
import DataTypes
import HelperFunctions
import Preprocess
import Defunctionalize
import SMTformatter
import Control.Monad.State
import Options.Applicative
import Data.Semigroup ((<>))
import System.IO 

-- Command line options

-- The input source is either a file or the standard input. 
data Input = FileInput FilePath | StdInput

-- The output format is (1) the monotone problems's format, (2) the extended
-- SMT-LIB2 format, or (3) the pure SMT-LIB2 format. 
data Format = MonoFormat | ExtendedSMT | PureSMT

data Options = Options
  { optInput :: Input
  , optOutputFormat :: Format }

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Read from FILENAME" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> short 's'
  <> help "Read from the standard input" )

monotoneFormat :: Parser Format
monotoneFormat = flag' MonoFormat
  (  long "monotone"
  <> short 'm'
  <> help "Output in the monotone problems' format")

extendedFormat :: Parser Format
extendedFormat = flag' ExtendedSMT
  (  long "extended"
  <> short 'e'
  <> help "Output in the extended SMT-LIB2 format")

pureFormat :: Parser Format
pureFormat = flag' PureSMT
  (  long "pure"
  <> short 'p'
  <> help "Output in the pure SMT-LIB2 format")

inputSource :: Parser Input
inputSource = fileInput <|> stdInput

outputFormat :: Parser Format
outputFormat = monotoneFormat <|> extendedFormat <|> pureFormat

parseOptions :: Parser Options
parseOptions = Options <$> inputSource <*> outputFormat

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- Main

main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "Defunctionalization of monotone problems")

run :: Options -> IO()
run (Options StdInput format) = do
  input <- getContents
  produceOutput input format
run (Options (FileInput filename) format) = do
  handle <- openFile filename ReadMode  
  input <- hGetContents handle
  produceOutput input format

produceOutput :: String -> Format -> IO()
produceOutput input MonoFormat = do
  let 
    tokens = alexScanTokens input
    source = parse tokens
    target = fst $ runState (produceTargetProblem source) freshVars
  print target
produceOutput input ExtendedSMT = do
  let 
    tokens = alexScanTokens input
    source = parse tokens
    target = fst $ runState (produceTargetProblemSMTExt source) freshVars
  putStr target
produceOutput input PureSMT = do
  let 
    tokens = alexScanTokens input
    source = parse tokens
    target = fst $ runState (produceTargetProblemSMTPure source) freshVars
  putStr target

-- Combine preprocessing and defunctionalization

produceTargetProblem :: MonoProblem -> State [String] MonoProblem
produceTargetProblem (MProblem sortEnv eqs goal) = do
  (eqs', goal') <- preprocess eqs goal sortEnv
  let applys = recursiveCreateApplys eqs'
  iomatches <- recursiveDefunctionalize eqs'
  finalGoal <- defunctionalizeBase goal'
  let finalEqs = iomatches ++ applys
      finalEnvVars = map (\(var, term) -> var) finalEqs
      finalSortEnv  = [(v, s) | (v, s) <- candidateSortEnv, v `elem` finalEnvVars]
  return (MProblem finalSortEnv finalEqs finalGoal)

produceTargetProblemSMTExt :: MonoProblem -> State [String] String
produceTargetProblemSMTExt s@(MProblem sortEnv eqs goal) = do
  t <- produceTargetProblem s
  translateSMTExt sortEnv t

produceTargetProblemSMTPure :: MonoProblem -> State [String] String
produceTargetProblemSMTPure s@(MProblem sortEnv eqs goal) = do
  t <- produceTargetProblem s
  return (translateSMTPure sortEnv t)

candidateSortEnv :: Env
candidateSortEnv = candidateSortEnvApplys ++ candidateSortEnvIOMacthes

candidateSortEnvApplys :: Env
candidateSortEnvApplys = [("Apply_" ++ show s, applySort s) | s <- [IntSort, BoolSort, ClosrSort]]

candidateSortEnvIOMacthes :: Env
candidateSortEnvIOMacthes = [("IOMatch_" ++ show s, iomatchSort s) | s <- [IntSort, BoolSort, ClosrSort]]

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

-- Translating target monotone problems into the extended SMT-LIB2 format

translateSMTExt :: Env -> MonoProblem -> State [String] String
translateSMTExt sourceEnv (MProblem finalSortEnv finalEqs finalGoal) = do
  let applys = groupEquations candidateSortEnvApplys finalEqs
      iomatches = groupEquations candidateSortEnvIOMacthes finalEqs
      declarationData = dataDeclaration (map fst sourceEnv)
      signature = defineSignature sourceEnv
  applys' <- recursiveDefineApplys applys
  iomatches' <- recursiveDefineIOMatches iomatches
  let -- Goal needs to be negated.
      goal' = "(assert (not " ++ defineBody finalGoal ++ "))\n"
      footer = goal' ++ "(check-sat)\n"
  return (declarationData ++ signature ++ applys' ++ iomatches' ++ footer)

recursiveDefineApplys :: [((String, Sort), [Term])] -> State [String] String
recursiveDefineApplys [] = return ""
recursiveDefineApplys (((v, s), ts): ds) = do
  def <- defineApplyExt (v, s) ts
  defs <- recursiveDefineApplys ds
  return (def ++ defs)

recursiveDefineIOMatches :: [((String, Sort), [Term])] -> State [String] String
recursiveDefineIOMatches [] = return ""
recursiveDefineIOMatches (((v, s), ts): ds) = do
  def <- defineIOMatchExt (v, s) ts
  defs <- recursiveDefineIOMatches ds
  return (def ++ defs)

-- Translating target monotone problems into the pure SMT-LIB2 format

translateSMTPure :: Env -> MonoProblem -> String
translateSMTPure sourceEnv (MProblem finalSortEnv finalEqs finalGoal) = 
  let setLogic = "(set-logic HORN)\n"
      applys = groupEquations candidateSortEnvApplys finalEqs
      iomatches = groupEquations candidateSortEnvIOMacthes finalEqs
      declarationData = dataDeclaration (map fst sourceEnv)
      signature = defineSignature sourceEnv
      applys' = unlines $ map (\(v,ts) -> defineRelationalVariablePure v ts) applys
      iomatches' = unlines $ map (\(v,ts) -> defineRelationalVariablePure v ts) iomatches
      -- Goal needs to be negated. 
      goal' = "(assert (not " ++ defineBody finalGoal ++ "))\n"
      footer = goal' ++ "(check-sat)\n"
  in (setLogic ++ declarationData ++ signature ++ applys' ++ iomatches' ++ footer)

groupEquations :: Env -> [Equation] -> [((String, Sort), [Term])]
groupEquations env eqs = filter (not. null. snd) (map (group eqs) env)
  where group list (var, sort) = ((var, sort), [t | (v, t) <- list, var == v])
