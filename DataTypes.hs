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
          deriving (Show, Eq)

type Equation = (String, Term)
type Env = [(String, Sort)]