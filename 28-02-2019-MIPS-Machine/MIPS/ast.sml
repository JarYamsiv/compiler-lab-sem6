(* The abstract syntax tree for expression *)
structure Ast = struct

 
datatype Expr  =  Const of int
		   		| EVar of string 
		   		| ARVar of string*Expr
	       		| Op    of Expr * BinOp * Expr
         

     and BinOp = Plus
	       | Minus
	       | Mul

datatype Condition =  CondOp of Expr*ConditionOp*Expr

	 and ConditionOp = EQ
	 		| GT
	 		| LT
	 		| GTEQ
	 		| LTEQ




datatype Statement =  As    of string * Expr
          | FnCl  of string
          | If of Condition*Statement list
          | IfEl of Condition*Statement list*Statement list


datatype Function = Fun of string* Statement list


datatype ProgramElement = St of Statement
						| Fn of Function 




fun binOpToString Plus  = "+"
  | binOpToString Minus = "-"
  | binOpToString Mul   = "*"

fun   condOpToString EQ    = "=="
 	| condOpToString LT    = "<"
 	| condOpToString GT    = ">"
 	| condOpToString GTEQ  = ">="
  	| condOpToString LTEQ  = "<="



fun plus  a b = Op (a, Plus, b)
fun minus a b = Op (a, Minus, b)
fun mul   a b = Op (a, Mul, b)

fun eq    a b = CondOp(a,EQ,b)
fun lt    a b = CondOp(a,LT,b)
fun gt    a b = CondOp(a,GT,b)



end
