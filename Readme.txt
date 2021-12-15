# Mini-EnsembleS
Abstract Syntax Tree -> AST.hs
Parser -> Parser.hs
TypeChecker -> TypeChecker.hs

Main.hs is the main script to execute the code.
To compile, run
> ghc Main.hs -o typechecker

Script takes in a filename as argument and performs parsing and typechecking. Usage:
> ./typechecker [filename]
If only parsing is to be executed, use option --parse:
> ./typechecker --parse [filename]

All input files discussed in the report are available in Input directory.
Concrete syntax is attached as well for reference.
