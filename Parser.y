{
module Main 
where
import System.IO
import Data.Char
import Control.Monad
import Compiler
import Assembler
}


%name calc
%tokentype { Token }
%error { parseError }

%right '=' 
%left '>' '<' '>=' '<=' '==' '!='
%left ';'
%left '||'
%left '&&'
%left '+' '-'
%left '*' '/'

%token 
    int             {TokenInt  $$     }
    id              {TokenId   $$     }
    while           {TokenWhile       }
    if              {TokenIf          }
    else            {TokenElse        }
    '+'             {TokenPlus        }
    '*'             {TokenMult        }
    '-'             {TokenMinus       }
    '/'             {TokenDiv         }
    '&&'            {TokenAnd         }
    '||'            {TokenOr          }
    '='             {TokenEqual       }
    '('             {TokenOB          }
    ')'             {TokenCB          }
    '{'             {TokenOC          }
    '}'             {TokenCC          }
    '>'             {TokenGreater     }
    '<'             {TokenLesser      }
    '!='            {TokenNotEquals   }
    '=='            {TokenEquals      }
    '>='            {TokenGreaterEq   }
    '<='            {TokenLesserEq    }
    ';'             {TokenEOS         }    

%%
Cmd  
  : if Expr  '{'Cmd'}' else '{'Cmd'}'                 { If $2 $4 $8 }
    | if Expr '{' Cmd '}'                         {IfUniq $2 $4 }
    | while Expr '{' Cmd '}'                                { While $2 $4 }
    | Cmd ';' Cmd                                       { Seq $1 $3 }
    | Cmd ';'                                           { UniqueSeq $1 }
    | id '=' Expr                                      { Atrib $1 $3 }

Expr  : Expr '==' Expr                                   { Equals $1 $3 }
      | Expr '!=' Expr                                   { Dif $1 $3 }
      | Expr '<' Expr                                    { Lesser $1 $3 }
      | Expr '>' Expr                                    { Greater $1 $3 }
      | Expr '<=' Expr                                   { LEq $1 $3 }
      | Expr '>=' Expr                                   { GEq $1 $3 }
      | Expr '+' Expr                                    { Plus $1 $3 }
      | Expr '-' Expr                                    { Minus $1 $3 }
      | Expr '*' Expr                                    { Mult $1 $3 }
      | Expr '/' Expr                                    { Div $1 $3  }
      | int                                              { Int $1 }
      | id                                              { Var $1 }
      | '(' Expr ')'                                     { Brack $2 }

{

parseError :: [Token] -> a
parseError _ = error "You sure test case is correct?" 


data Token
    = TokenInt Int
    | TokenId String
    | TokenIf
    | TokenElse
    | TokenWhile
    | TokenPlus
    | TokenMinus
    | TokenMult
    | TokenDiv
    | TokenAnd
    | TokenOr
    | TokenEqual
    | TokenGreater
    | TokenLesser
    | TokenLesserEq
    | TokenGreaterEq
    | TokenEquals
    | TokenOB
    | TokenCB
    | TokenOC
    | TokenCC
    | TokenEOS
    | TokenNotEquals
    deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isDigit c = lexNum (c:cs)
      | isAlpha c = lexVar(c:cs)
      
lexer   ('+':cs)      =     TokenPlus       : lexer cs
lexer   ('*':cs)      =     TokenMult       : lexer cs
lexer   ('-':cs)      =     TokenMinus      : lexer cs
lexer   ('/':cs)      =     TokenDiv        : lexer cs
lexer   ('!':'=':cs)  =     TokenNotEquals  : lexer cs
lexer   ('=':'=':cs)  =     TokenEquals     : lexer cs
lexer   ('=':cs)      =     TokenEqual      : lexer cs
lexer   ('(':cs)      =     TokenOB         : lexer cs
lexer   (')':cs)      =     TokenCB         : lexer cs
lexer   ('{':cs)      =     TokenOC         : lexer cs
lexer   ('}':cs)      =     TokenCC         : lexer cs
lexer   ('>':'=':cs)  =     TokenGreaterEq  : lexer cs
lexer   ('<':'=':cs)  =     TokenLesserEq   : lexer cs
lexer   ('>':cs)      =     TokenGreater    : lexer cs
lexer   ('<':cs)      =     TokenLesser     : lexer cs
lexer   (';':cs)      =     TokenEOS        : lexer cs
lexer   ('&':'&':cs)  =     TokenAnd        : lexer cs
lexer   ('|':'|':cs)  =     TokenOr         : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs = case span isAlpha cs of
        ("else" ,rest)   -> TokenElse               : lexer rest
        ("if"   ,rest)   -> TokenIf                 : lexer rest
        ("while",rest)   -> TokenWhile              : lexer rest
        (var     ,rest)   -> TokenId   (var)          : lexer rest
             
main = do
        

        putStrLn "Input file: "
        ifile <- getLine
        putStrLn "Output file: "
        ofile <- getLine
        result <- fmap (emit.compile.calc.lexer) (readFile ifile)
        
        --print result
                
        writeFile ofile result
      

}  