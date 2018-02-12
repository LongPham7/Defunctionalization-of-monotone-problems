{
module Parser where
import DataTypes
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  environment  {TokenEnvDec}
  var          {TokenVar $$}
  ':'          {TokenColon}
  intSort      {TokenIntSort}
  boolSort     {TokenBoolSort}
  '->'         {TokenArrow}
  '('          {TokenLeftParen}
  ')'          {TokenRightParen}

  program      {TokenProgramDec}
  E            {TokenExists}
  ':='         {TokenDef}
  ';'          {TokenEndMarker}
  '\\'         {TokenLambda}
  '.'          {TokenPeriod}
  '&&'         {TokenAnd}
  '||'         {TokenOr}
  '+'          {TokenAdd}
  '-'          {TokenSub}
  '<'          {TokenSmaller}
  '<='         {TokenSmallerEq}
  '='          {TokenEq}
  '>='         {TokenLarger}
  '>'          {TokenLargerEq}
  int          {TokenNum $$}

  goal         {TokenGoalDec}


%nonassoc '<' '<=' '=' '>' '>='
%right '->'
%left '+' '-'
%left '&&'
%left '||'

%%

MonotoneProblem : environment Env program Definite goal Goal { MProblem $2 $4 $6 }

Env :: { [(String, Sort)] }
Env : {- empty -} { [] }
    | Env Judgement { $2:$1 }

Judgement :: { (String, Sort) }
Judgement : var ':' Sort { ($1, $3) }

Sort :: { Sort }
Sort : intSort { IntSort }
     | boolSort { BoolSort }
     | '(' Sort ')' { $2 }
     | Sort '->' Sort { Arrow $1 $3 }

Definite :: { [(String, Term)] }
Definite : {- empty -} { [] }
         | Definite Equation ';' { $2:$1 }

Equation :: { (String, Term) }
Equation : var ':=' RelationalGoalTerm { ($1,$3) }

RelationalGoalTerm :: { Term }
RelationalGoalTerm : FirstOrderTerm { $1 }
                   | FirstOrderFormula { $1 }
                   | CompositeRelationalGoalTerm { $1 }
                   -- With parentheses
                   | '(' CompositeRelationalGoalTerm ')' { $2 }

CompositeRelationalGoalTerm :: { Term }
CompositeRelationalGoalTerm : RelationalGoalTerm '&&' RelationalGoalTerm { And $1 $3 }
                            | RelationalGoalTerm '||' RelationalGoalTerm { Or $1 $3 }
                            | '\\' var ':' Sort '.' '(' RelationalGoalTerm ')' { Lambda $2 $4 $7 }
                            | E var ':' Sort '.' '(' RelationalGoalTerm ')' { Exists $2 $4 $7 }
                            | RelationalGoalTerm RelationalGoalTerm { App $1 $2 }

FirstOrderFormula :: {Term}
FirstOrderFormula : FirstOrderTerm '<' FirstOrderTerm { Sma $1 $3 }
                  | FirstOrderTerm '<=' FirstOrderTerm { SmaEq $1 $3 }
                  | FirstOrderTerm '=' FirstOrderTerm { Eq $1 $3 }
                  | FirstOrderTerm '>' FirstOrderTerm { Lar $1 $3 }
                  | FirstOrderTerm '>=' FirstOrderTerm { LarEq $1 $3 }
                  | '(' FirstOrderFormula ')' { $2 }

FirstOrderTerm :: { Term }
FirstOrderTerm : var { Var $1 }
               | int { Num $1 }
               | FirstOrderTerm '+' FirstOrderTerm { Add $1 $3 }
               | FirstOrderTerm '-' FirstOrderTerm { Sub $1 $3 }
               | '(' FirstOrderTerm ')' { $2 }

Goal :: { Term }
Goal : RelationalGoalTerm { $1 }

{
parseError :: [Token] -> a
parseError s = error (show s)
}