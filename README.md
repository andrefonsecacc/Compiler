# Compiler
Work for Compiler course , it is a simple Compiler for C0 progamming language.

This is a implementation of parser for a subset of the language
C0 programming ( http://c0.typesafety.net/tutorial/ ) . The subset of
language  include :
  - Integers
  - Boolean 
  - Arithmetic expressions
  - Boolean expressions
  - Assignments
  - If then else
  - While Cycles
 
The parser do syntactical analysis of the program in C0 building
a representation of abstract tree (date of Haskell ).

Then the structure built is translated to program code through 3 address 
( Http://en.wikipedia.org/wiki/Three_address_code ) .

From the 3 address code structure the program translate the corresponding MIPS code printing him to a file.

How to run : 

happy Parser.y  
ghc Parser.hs 
./Parser

It will ask for a file with C0 code . Example : 

b=1;
a=2;

if(a==b){
    a=10;
    }
    
while(a<b){
    a=5;
}


 
