module Compiler where

data Cmd 
        = UniqueSeq Cmd
        | Seq Cmd Cmd
        | While Expr Cmd
        | IfUniq Expr Cmd
        | If Expr Cmd Cmd
        | Atrib String Expr
        deriving (Show)
data Expr
        = Plus Expr Expr
        | Mult        Expr     Expr
        | Div         Expr     Expr
        | Minus       Expr     Expr 
        | Greater         Expr     Expr
        | Lesser          Expr     Expr
        | GEq       Expr     Expr
        | LEq        Expr     Expr
        | Equals     Expr     Expr
        | Dif       Expr Expr
        | Int Int
        | Var String
        | Brack Expr
        deriving (Show)


data MipsCode 
            = Label String
            | GoTo String
            | IfFalse Expr String
            | AtribMips String Expr
            deriving (Show)


--newVar
newVar :: Int -> String
newVar n = "t" ++ (show n)

--newLabel
newlabel :: Int -> String
newlabel n = "L" ++(show n)


-- Symbol list

findCreateReg :: [(String , String  , Int)] -> [(String , String , Int)] -> String  -> String -> ([(String, String , Int)])
findCreateReg _ [] a var = ([(var,a,0)])
findCreateReg ((s,t,i):[]) regs a var   | s==var = regs 
                    | otherwise = (regs ++ [(var , a, i+1)])
findCreateReg ((s,t,i):rest) regs a var | s==var = regs
                    | otherwise = findCreateReg rest regs a var 

findReg :: [(String,String,Int)]-> String -> String
findReg [] var = error "reg does not exist"
findReg ((s,t,i):rest) var  |s == var = ("s" ++ (show i))
                            |otherwise = findReg rest var

findRegType :: [(String,String,Int)]-> String -> String
findRegType [] var = error "reg does not exist"
findRegType ((s,t,i):rest) var  |s == var = t
                            |otherwise = findReg rest var
--end Symbol list


compile:: Cmd -> (Int, Int,[(String,String,Int)], [MipsCode])
compile cmd = compileCmd cmd 0 0 []

--compile cmds
compileCmd :: Cmd -> Int -> Int -> [(String,String,Int)]-> (Int, Int,[(String,String,Int)], [MipsCode])

--UNIQUE COMMAND
compileCmd (UniqueSeq cmd1) k l regs = let
                        (v0, t0,regs1, l0) = compileCmd cmd1 k l regs 
                         in (v0, t0 + 1,regs, l0)
                        
                        

--SEQUENCE OF COMMANDS
compileCmd (Seq cmd1 cmd2) k l regs = let
                        (v0, t0,regs1, l0) = compileCmd cmd1 k l regs
                        (v1, t1,regs2, l1) = compileCmd cmd2 v0 t0 regs1
                        in (v1, t1 + 1,regs2, l0 ++ l1)

--WHILE CONDITION
compileCmd (While expr cmd) k l regs = let
                        labelBeginWhile = newlabel l
                        labelEndWhile = newlabel (l + 1)
                        (v0, t0, l0) = compileExpr expr k regs
                        (v1, t1,regs1, l1) = compileCmd cmd v0 (l + 2) regs
                        in (v1, t1 + 1,regs1,[Label labelBeginWhile] ++ l0 ++ [IfFalse t0 labelEndWhile] ++ l1 ++ [GoTo labelBeginWhile] ++  [Label labelEndWhile])

--IF CONDITION
compileCmd (IfUniq expr cmd1) k l regs = let
                        labelElse = newlabel l
                        labelEndIf = newlabel (l + 1)
                        (v0, t0, l0) = compileExpr expr k regs
                        (v1, t1,regs1, l1) = compileCmd cmd1 v0 (l + 2) regs
                        in (v0, t1 + 1,regs1, l0 ++ [IfFalse t0 labelElse] ++ l1 ++ [GoTo labelEndIf])

compileCmd (If expr cmd1 cmd2) k l regs = let
                        labelElse = newlabel l
                        labelEndIf = newlabel (l + 1)
                        (v0, t0, l0) = compileExpr expr k regs
                        (v1, t1,regs1, l1) = compileCmd cmd1 v0 (l + 2) regs
                        (v2, t2,regs2, l2) = compileCmd cmd2 v1 t1 regs1
                        in (v2, t2 + 1,regs2, l0 ++ [IfFalse t0 labelElse] ++ l1 ++ [GoTo labelEndIf] ++ [Label labelElse] ++ l2 ++ [Label labelEndIf])

--ATRIBUTION OF AN EXPR TO A VAR
--not sure

compileCmd (Atrib var expr) k l regs = let
                        (v0, t0, l0) = compileExpr expr k regs
                        regs1 = findCreateReg regs regs "int" var
                        t =findReg regs1 var 
                        in (v0 + 1, l,regs1, l0 ++ [AtribMips t t0])

{-
compileCmd (Atrib var (Int n)) k l regs = let
            (v0, t0, l0) = compileExpr (Int n) k regs
            regs1= findCreateReg regs regs "int" var
            t=findReg regs1 var
            in (v0 + 1, l,regs1, l0 ++ [AtribMips t t0])
-}

--COMPILE EXPRESSIONS
compileExpr :: Expr -> Int ->[(String,String,Int)] -> (Int, Expr,[MipsCode])
compileExpr (Int n) k regs = (k, Int n, [])
compileExpr (Var x) k regs = (k, Var (findReg regs x), [])
compileExpr (Brack expr) k  regs = compileExpr expr k regs --not sure

--ADITION
compileExpr (Plus e1 e2) k regs = let
                        (v1, t1, l1) = compileExpr e1 k regs
                        (v2, t2, l2) = compileExpr e2 v1 regs
                        t = newVar v2 
                        in (v2 + 1, (Var t), l2 ++ l1 ++ [AtribMips t (Plus t1 t2)])

--SUBTRACTION
compileExpr (Minus e1 e2) k regs = let
                        (v1, t1, l1) = compileExpr e1 k regs
                        (v2, t2, l2) = compileExpr e2 v1 regs
                        t = newVar v2 in  (v2 + 1, (Var t), l2 ++ l1 ++ [AtribMips t (Minus t1 t2)])

--MULTIPLICATION
compileExpr (Mult e1 e2) k regs = let
                        (v1, t1, l1) = compileExpr e1 k regs
                        (v2, t2, l2) = compileExpr e2 v1 regs
                        t = newVar v2 
                        in  (v2 + 1, (Var t), l2 ++ l1 ++ [AtribMips t (Mult t1 t2)])

--DIVISION
compileExpr (Div e1 e2) k regs = let
                        (v1, t1, l1) = compileExpr e1 k regs
                        (v2, t2, l2) = compileExpr e2 v1 regs
                        t = newVar v2 
                        in  (v2 + 1, (Var t), l2 ++ l1 ++ [AtribMips t (Div t1 t2)])

--COMPARATION
compileExpr (Equals e1 e2) k regs = let
                        (v1, t1, l1) = compileExpr e1 k regs
                        (v2, t2, l2) = compileExpr e2 v1 regs
                        t = newVar v2 
                        in  (v2 + 1, (Var t), l2 ++ l1 ++ [AtribMips t (Equals t1 t2)])

--DIFFERENCE
compileExpr (Dif e1 e2) k regs = let
                        (v1, t1, l1) = compileExpr e1 k regs
                        (v2, t2, l2) = compileExpr e2 v1 regs
                        t = newVar v2 
                        in  (v2 + 1, (Var t), l2 ++ l1 ++ [AtribMips t (Dif t1 t2)])

--LOWER THAN
compileExpr (Lesser e1 e2) k regs = let
                        (v1, t1, l1) = compileExpr e1 k regs
                        (v2, t2, l2) = compileExpr e2 v1 regs
                        t = newVar v2 
                        in  (v2 + 1, (Var t), l2 ++ l1 ++ [AtribMips t (Lesser t1 t2)])

--GREATER THAN
compileExpr (Greater e1 e2) k regs = let
                        (v1, t1, l1) = compileExpr e1 k regs
                        (v2, t2, l2) = compileExpr e2 v1 regs
                        t = newVar v2 
                        in  (v2 + 1, (Var t), l2 ++ l1 ++ [AtribMips t (Greater t1 t2)])

--LOWER OR EQUAL
compileExpr (LEq e1 e2) k  regs= let
                        (v1, t1, l1) = compileExpr e1 k regs
                        (v2, t2, l2) = compileExpr e2 v1 regs
                        t = newVar v2 
                        in  (v2 + 1, (Var t), l2 ++ l1 ++ [AtribMips t (LEq t1 t2)])

--GREATER OR EQUAL
compileExpr (GEq e1 e2) k regs = let
                        (v1, t1, l1) = compileExpr e1 k regs
                        (v2, t2, l2) = compileExpr e2 v1 regs
                        t = newVar v2 
                        in  (v2 + 1, (Var t), l2 ++ l1 ++ [AtribMips t (GEq t1 t2)])