module DataTypes where

data Token = TokenEnvDec
           | TokenProgramDec
           | TokenGoalDec
           | TokenExists
           | TokenDef
           | TokenEndMarker
           | TokenLambda
           | TokenPeriod
           | TokenAnd
           | TokenOr
           | TokenAdd
           | TokenSub
           | TokenSmaller
           | TokenSmallerEq
           | TokenLarger
           | TokenLargerEq
           | TokenEq
           | TokenNum Int
           | TokenVar String
           | TokenColon
           | TokenIntSort
           | TokenBoolSort
           | TokenArrow
           | TokenLeftParen
           | TokenRightParen
           deriving (Show, Eq)

type Equation = (String, Term)
type Env = [(String, Sort)]

-- Monotone problems

data MonoProblem = MProblem Env [Equation] Term
                   deriving (Eq)

instance Show MonoProblem where
  show = showMonoProblem

showMonoProblem :: MonoProblem -> String
showMonoProblem (MProblem sortEnv eqs goal) = 
  let envString = "environment\n" ++ showEnv sortEnv
      eqsString = "program\n" ++ showEqs eqs
      goalString = "goal\n" ++ show goal
  in envString ++ "\n" ++ eqsString ++ "\n" ++ goalString

showEnv :: Env -> String
showEnv env = concat $ map showJudgement env
  where showJudgement (var, sort) = var ++ ": " ++ show sort ++ "\n"

showEqs :: [Equation] -> String
showEqs eqs = concat $ map showEq eqs
  where showEq (var, term) = var ++ " := " ++ show term ++ "\n"

-- Sorts

data Sort = IntSort | BoolSort | ClosrSort | Arrow Sort Sort
            deriving (Eq)

instance Show Sort where
  show = showSort

showSort :: Sort -> String
showSort IntSort = "Int"
showSort BoolSort = "Bool"
showSort ClosrSort = "Closr"
showSort (Arrow s@(Arrow t1 t2) t3) = "(" ++ showSort s ++ ") -> " ++ showSort t3
showSort (Arrow s t) = showSort s ++ " -> " ++ showSort t

-- Goal terms

-- Var, TopVar, App, and Lambda are used in a source monotone problem. These are
-- replaced with VarSort, TopVarSort, AppSort, and LambdaSort, respectively,
-- during type annotation.
data Term = Var String | Num Int | Const String Sort
          | TopVarSort String Sort
          | VarSort String Sort
          | App Term Term
          | AppSort Term Term Sort
          | And Term Term
          | Or Term Term
          | Lambda String Sort Term
          | LambdaSort String Sort Term Sort -- The second sort is the sort of the body of the abstraction
          | Exists String Sort Term
          | Sma Term Term
          | SmaEq Term Term
          | Eq Term Term
          | Lar Term Term 
          | LarEq Term Term
          | Add Term Term
          | Sub Term Term
          deriving (Eq)

instance Show Term where
  show = showTerm

showTerm :: Term -> String
showTerm (Var v) = v
showTerm (TopVarSort v s) = v
showTerm (VarSort v s) = v
showTerm (Num n) = show n
showTerm (Const c s) = c
showTerm (App u v) = "(" ++ showTerm u ++ " " ++ showTerm v ++ ")"
showTerm (AppSort u v s) = "(" ++ showTerm u ++ " " ++ showTerm v ++ ")"
showTerm (And u v) = "(" ++ showTerm u ++ " && " ++ showTerm v ++ ")"
showTerm (Or u v) = "(" ++ showTerm u ++ " || " ++ showTerm v ++ ")"
showTerm (Lambda v s b) = "(Lambda " ++ v ++ ": " ++ show s ++ ". " ++ showTerm b ++ ")"
showTerm (LambdaSort v s1 b s2) = "(Lambda " ++ v ++ ": " ++ show s1 ++ ". " ++ showTerm b ++ ")"
showTerm (Exists v s b) = "(E " ++ v ++ ": " ++ show s ++ ". " ++ showTerm b ++ ")"
showTerm (Sma u v) = showTerm u ++ " < " ++ showTerm v
showTerm (SmaEq u v) = showTerm u ++ " <= " ++ showTerm v
showTerm (Eq u v) = showTerm u ++ " = " ++ showTerm v
showTerm (Lar u v) = showTerm u ++ " > " ++ showTerm v
showTerm (LarEq u v) = showTerm u ++ " >= " ++ showTerm v
showTerm (Add u v) = "(" ++ showTerm u ++ " + " ++ showTerm v ++ ")"
showTerm (Sub u v) = "(" ++ showTerm u ++ " - " ++ showTerm v ++ ")"
