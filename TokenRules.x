{
module Tokeniser where
import DataTypes
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@id = ([A-DF-Za-z][A-Za-z'_]*) | (E[A-Za-z'_]+)

tokens :-

  -- Whitespace insensitive
  $white+      ;

  -- Comments start with #
  \#.*         ;

  -- Declaration
  environment  {\s -> TokenEnvDec}
  program      {\s -> TokenProgramDec}
  goal         {\s -> TokenGoalDec}

  -- Sort
  int          {\s -> TokenIntSort}
  bool         {\s -> TokenBoolSort}

  -- Sort environment
  @id          {\s -> TokenVar s}
  :            {\s -> TokenColon}
  "->"         {\s -> TokenArrow}
  \(           {\s -> TokenLeftParen}
  \)           {\s -> TokenRightParen}

  -- Definite formula component
  E            {\s -> TokenExists}
  ":="         {\s -> TokenDef}
  ";"          {\s -> TokenEndMarker}
  \\           {\s -> TokenLambda}
  \.           {\s -> TokenPeriod}
  "&&"         {\s -> TokenAnd}
  "||"         {\s -> TokenOr}
  "+"          {\s -> TokenAdd}
  "-"          {\s -> TokenSub}
  "<"          {\s -> TokenSmaller}
  "<="         {\s -> TokenSmallerEq}
  "="          {\s -> TokenEq}
  ">="         {\s -> TokenLarger}
  ">"          {\s -> TokenLargerEq}
  $digit+      {\s -> TokenNum (read s)}