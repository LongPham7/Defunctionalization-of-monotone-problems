# Defunctionalization of monotone logic program safety problems

This is a Haskell program that takes in the specification of a monotone logic program safety problem (often abbreviated as a monotone problem) and produces the result of defunctionalizing the input problem. For simplicity, the constraint language is restricted to the one of integers with addition and subtraction. 

## Background

Formal verification is a field of computer science that concerns verifying the correctness of computer programs. The correctness of programs is defined with respect to user-specified properties. Hence, a verification problem consists of two components:
1. A program
2. A property that we want to verify whether the program satisfies.

It is natural to express properties to be verified as logical statements. By contrast, computer programs in practice are expressed using programming languages (e.g. Java, Prolog, Haskell ). However, when we study verification of programs, it is convenient to use logic to express programs as well as properties to be verified. This allows us to have a unifying framework to deal with both properties and programs. 

Monotone problems are examples of such logic-based representations of verification problems. A monotone problem is given as a triple (&#916;, P, G), where &#916; is a sort environment, P is a logic program, and G is a goal formula. P represents a higher-order program, which can contain higher-order functions. 

Although verification of first-order programs has been extensively studied, not all techniques developed for verification of first-order programs are applicable to higher-order programs because higher-order programs present some challenges that do not exist in first-order programs. To bridge the gap between higher-order verification problems and first-order ones, I developed an algorithm based on John Reynold's defunctionalization to convert higher-order monotone problems to first-order ones.

For more information about verification of monotone problems, please refer to *Higher-Order Constrained Horn Clauses and Refinement Types* by Toby Cathcart Burn, C.-H Luke Ong, and Steven Ramsay. 

## Run

### Installation
First, install a basic Haskell platform that comes with GHC, Cabal, Stack, and the basic Haskell packages. Then install the packages alex (for lexer generation) and happy (for parser generation) by

    cabal install alex
    cabal install happy

or

    stack install alex
    stack install happy

### Execution
First run 

    alex -o Tokeniser.hs TokenRules.x
    happy -o Parser.hs Grammar.y

The output Haskell code is placed in Tokeniser.hs and Parser.hs. The names of these files must be exact. 

To compile the Haskell source code, run

    ghc -O Main.hs

The command line option `-O` enables compilation optimisation. An executable file is generated and placed in the root directory. 

To execute this executable file, run

    .\Main -f SampleInput.txt

Here, the option `-f` allows the user to specify an input file name. If you want to use the standard input, use the option `-s`.

Another option is `-z`, which displays a target monotone problem (i.e. a defunctionalized monotone problem) in the extended SMT-LIB2 format that is compatible with Z3. 

If you need any help with command line options, use the `-h` options. 

## Sample inputs

A sample source monotone problem is available in SampleInput.txt. The folder "Sample inputs"
contains additional source monotone problems. The samples in "Sample inputs" are obtained by manually
translating some of the Horn clause problems in Toby Cathcart Burn's 
[implementation project](https://github.com/penteract/HigherOrderHornRefinement) to monotone problems.
His samples are originally from [MoCHi](http://www-kb.is.s.u-tokyo.ac.jp/~ryosuke/mochi/). 

The algorithm for turning Horn clause problems into monotone problems is 
described in *Higher-Order Constrained Horn Clauses and Refinement Types* by Toby Cathcart Burn, C.-H Luke Ong, and Steven Ramsay.

## Input format

The **environment** section is a list of bindings of top-level relational variables and sorts. Each binding may span multiple lines. 

The **program** section is a list of definitions of top-level relational variables. Each definition comprises a top-level relational variable on the left hand side of ':=' and a goal term on the right hand side. Each definition must end with a semicolon. A detailed grammar of goal terms can be found in Grammar.y.

The **goal** consists of a single goal term of the propositional sort. 

One-line comments begin with #. Multiline comments are not supported. 