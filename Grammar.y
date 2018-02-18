-- The specification of the grammar is attributed to Steven Ramsay.

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


%nonassoc '.'
%right '->'
%left '||'
%left '&&'
%nonassoc '<' '<=' '=' '>' '>='
%left '+' '-'

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
Equation : var ':=' Term { ($1,$3) }

Term :: { Term }
Term : Term '&&' Term { And $1 $3 }
     | Term '||' Term { Or $1 $3 }
     | E var ':' Sort '.' Term { Exists $2 $4 $6 }
     | '\\' var ':' Sort '.' Term { Lambda $2 $4 $6 }
     | ApplicativeTerm { $1 }
     | FirstOrderFormula { $1 }
     | Term '+' Term { Add $1 $3 }
     | Term '-' Term { Sub $1 $3 }
     | '(' Term ')' { $2 }  
               
ApplicativeTerm :: { Term }
ApplicativeTerm : var { Var $1 }
                | int { Num $1 }
                | '(' Term ')' '(' Term ')' { App $2 $5 }
                | '(' Term ')' var { App $2 (Var $4) }
                | '(' Term ')' int { App $2 (Num $4) }
                | ApplicativeTerm '(' Term ')' { App $1 $3 }
                | ApplicativeTerm var { App $1 (Var $2) }
                | ApplicativeTerm int { App $1 (Num $2) }

FirstOrderFormula :: { Term }
FirstOrderFormula : Term '<' Term { Sma $1 $3 }
                  | Term '<=' Term { SmaEq $1 $3 }
                  | Term '=' Term { Eq $1 $3 }
                  | Term '>' Term { Lar $1 $3 }
                  | Term '>=' Term { LarEq $1 $3 }

Goal :: { Term }
Goal : Term { $1 }

{
parseError :: [Token] -> a
parseError s = error (show s)
}