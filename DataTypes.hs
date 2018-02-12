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

data MonoProblem = MProblem [(String, Sort)] [(String, Term)] Term
                   deriving (Show, Eq)
  
data Sort = IntSort | BoolSort | ClosrSort | Arrow Sort Sort
            deriving (Show, Eq)
  
data Term = Var String | Num Int | Const String
          | TopVarSort String Sort
          | VarSort String Sort
          | App Term Term
          | AppSort Term Term Sort
          | And Term Term
          | Or Term Term
          | Lambda String Sort Term
          | LambdaSort String Sort Term Sort
          | Exists String Sort Term
          | Sma Term Term
          | SmaEq Term Term
          | Eq Term Term
          | Lar Term Term 
          | LarEq Term Term
          | Add Term Term
          | Sub Term Term
          deriving (Show, Eq)

type Equation = (String, Term)
type Env = [(String, Sort)]


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