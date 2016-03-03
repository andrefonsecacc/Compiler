module Assembler where
import Compiler

emit :: (Int,Int,[(String,String,Int)],[MipsCode]) -> String
emit (_,_,regs,[]) = ""
emit (x,y,regs,(e:r))=(emit_instr x e) ++ "\n" ++ (emit (x,y,regs,r))

emit_instr :: Int->MipsCode-> String

emit_instr k (AtribMips reg1 (Int n1)) = "li $" ++ reg1 ++ " ," ++ (show n1)
emit_instr k (AtribMips reg1 (Var v1)) = "move $" ++ reg1 ++ ", $" ++ v1
------------------------------------------------------------------------------------------------------------------------
emit_instr k (AtribMips reg1 (Plus (Var v1) (Var v2))) = "add $" ++ reg1 ++ ", $" ++ v1 ++ ", $" ++ v2
emit_instr k (AtribMips reg1 (Plus (Int n1) (Var v1))) = "addi $" ++ reg1 ++" ,$" ++ v1 ++ ", " ++ (show n1)
emit_instr k (AtribMips reg1 (Plus (Var v1) (Int n1))) = "addi $" ++ reg1 ++" ,$" ++ v1 ++ ", " ++ (show n1)
emit_instr k (AtribMips reg1 (Plus (Int n1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "addi $" ++ reg1 ++ ", $t"++ (show k) ++", " ++ (show n2)
-----------------------------------------------------------------------------------------------------------------------
emit_instr k (AtribMips reg1 (Minus (Var v1) (Var v2))) = "sub $" ++ reg1 ++", $" ++ v1 ++ ", $" ++ v2
emit_instr k (AtribMips reg1 (Minus (Int n1) (Var v2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "sub $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $" ++ v2
emit_instr k (AtribMips reg1 (Minus (Var v1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n2) ++ "\n"
														++ "sub $" ++ reg1 ++ ", $" ++ v1 ++ ", $t" ++ (show k)
emit_instr k (AtribMips reg1 (Minus (Int n1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n2) ++ "\n" 
														++ "sub $t" ++ (show k) ++ ", $zero, $t" ++ (show k) ++ "\n"
														++ "addi $" ++ reg1 ++ ", $t" ++ (show k) ++ ", " ++ (show n1)
-------------------------------------------------------------------------------------------------------------------------

emit_instr k (AtribMips reg1 (Mult (Var v1) (Var v2))) = "mult $" ++ reg1 ++" ,$" ++ v1 ++ " ,$" ++ v2
emit_instr k (AtribMips reg1 (Mult (Int n1) (Var v2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "mul $" ++ reg1 ++ ", $" ++ v2 ++ ", $t" ++ (show k)
emit_instr k (AtribMips reg1 (Mult (Var v1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n2) ++ "\n"
														++ "mul $" ++ reg1 ++ ", $" ++ v1 ++ ", $t" ++ (show k)
emit_instr k (AtribMips reg1 (Mult (Int n1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "li $t" ++ (show (k+1)) ++ ", " ++ (show n2) ++ "\n"
														++ "mul $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $t" ++ (show (k+1))
-----------------------------------------------------------------------------------------------------------------------------

emit_instr k (AtribMips reg1 (Div (Var v1) (Var v2))) = "div $" ++ reg1 ++", $" ++ v1 ++ " ,$" ++ v2
emit_instr k (AtribMips reg1 (Div (Int n1) (Var v2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "div $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $" ++ v2
emit_instr k (AtribMips reg1 (Div (Var v1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n2) ++ "\n"
														++ "div $" ++ reg1 ++ ", $" ++ v1 ++ ", $t" ++ (show k)
emit_instr k (AtribMips reg1 (Div (Int n1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "li $t" ++ (show (k+1)) ++ ", " ++ (show n2) ++ "\n"
														++ "div $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $t" ++ (show (k+1))
----------------------------------------------------------------------------------------------------------------------------------

emit_instr k (AtribMips reg1 (Equals (Var v1) (Var v2))) = "seq $"++ reg1 ++", $"++v1++", $"++v2
emit_instr k (AtribMips reg1 (Equals (Int n1) (Var v2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "seq $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $" ++ v2
emit_instr k (AtribMips reg1 (Equals (Var v1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n2) ++ "\n"
														++ "seq $" ++ reg1 ++ ", $" ++ v1 ++ ", $t" ++ (show k)
emit_instr k (AtribMips reg1 (Equals (Int n1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "li $t" ++ (show (k+1)) ++ ", " ++ (show n2) ++ "\n"
														++ "seq $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $t" ++ (show (k+1))
--------------------------------------------------------------------------------------------------------------------------------------------

emit_instr k (AtribMips reg1 (Dif (Var v1) (Var v2))) = "sne $"++ reg1 ++" ,$"++v1++" ,$"++v2
emit_instr k (AtribMips reg1 (Dif (Int n1) (Var v2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "sne $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $" ++ v2
emit_instr k (AtribMips reg1 (Dif (Var v1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n2) ++ "\n"
														++ "sne $" ++ reg1 ++ ", $" ++ v1 ++ ", $t" ++ (show k)
emit_instr k (AtribMips reg1 (Dif (Int n1) (Int n2))) =  "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "li $t" ++ (show (k+1)) ++ ", " ++ (show n2) ++ "\n"
														++ "sne $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $t" ++ (show (k+1))

----------------------------------------------------------------------------------------------------------------------------------------------------------
emit_instr k (AtribMips reg1 (Greater (Var v1) (Var v2))) = "sgt $"++ reg1 ++", $"++v1++" ,$"++v2
emit_instr k (AtribMips reg1 (Greater (Int n1) (Var v2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "sgt $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $" ++ v2
emit_instr k (AtribMips reg1 (Greater (Var v1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n2) ++ "\n"
														++ "sgt $" ++ reg1 ++ ", $" ++ v1 ++ ", $t" ++ (show k)
emit_instr k (AtribMips reg1 (Greater (Int n1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "li $t" ++ (show (k+1)) ++ ", " ++ (show n2) ++ "\n"
														++ "sgt $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $t" ++ (show (k+1))
---------------------------------------------------------------------------------------------------------------------------------------------------------
emit_instr k (AtribMips reg1 (GEq (Var v1) (Var v2))) = "sge $"++ reg1 ++", $"++v1++", $"++v2
emit_instr k (AtribMips reg1 (GEq (Int n1) (Var v2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "sge $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $" ++ v2
emit_instr k (AtribMips reg1 (GEq (Var v1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n2) ++ "\n"
														++ "sge $" ++ reg1 ++ ", $" ++ v1 ++ ", $t" ++ (show k)
emit_instr k (AtribMips reg1 (GEq (Int n1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "li $t" ++ (show (k+1)) ++ ", " ++ (show n2) ++ "\n"
														++ "sge $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $t" ++ (show (k+1))
--------------------------------------------------------------------------------------------------------------------------------------------------------------
emit_instr k (AtribMips reg1 (Lesser (Var v1) (Var v2))) = "slt $"++ reg1 ++", $"++v1++",$"++v2
emit_instr k (AtribMips reg1 (Lesser (Int n1) (Var v2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "stl $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $" ++ v2
emit_instr k (AtribMips reg1 (Lesser (Var v1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n2) ++ "\n"
														++ "stl $" ++ reg1 ++ ", $" ++ v1 ++ ", $t" ++ (show k)
emit_instr k (AtribMips reg1 (Lesser (Int n1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "li $t" ++ (show (k+1)) ++ ", " ++ (show n2) ++ "\n"
														++ "stl $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $t" ++ (show (k+1))
-------------------------------------------------------------------------------------------------------------------------------------------------------------
emit_instr k (AtribMips reg1 (LEq (Var v1) (Var v2))) = "sle $"++ reg1 ++", $"++v1++", $"++v2
emit_instr k (AtribMips reg1 (LEq (Int n1) (Var v2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "sle $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $" ++ v2
emit_instr k (AtribMips reg1 (LEq (Var v1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n2) ++ "\n"
														++ "sle $" ++ reg1 ++ ", $" ++ v1 ++ ", $t" ++ (show k)
emit_instr k (AtribMips reg1 (LEq (Int n1) (Int n2))) = "li $t" ++ (show k) ++ ", " ++ (show n1) ++ "\n"
														++ "li $t" ++ (show (k+1)) ++ ", " ++ (show n2) ++ "\n"
														++ "sle $" ++ reg1 ++ ", $t" ++ (show k) ++ ", $t" ++ (show (k+1))

--------------------------------------------------------------------------------------------
emit_instr k (Label lb) = lb ++ ":"

emit_instr k (GoTo lb) = "b " ++ lb

emit_instr k (IfFalse (Var v1) (lb)) = "beq $" ++ v1 ++ ", 0, " ++ lb
