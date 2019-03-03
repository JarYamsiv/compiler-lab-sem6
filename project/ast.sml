(* The abstract syntax tree for expression *)
structure Ast = struct

datatype Condition = CConst of int
			| CVar of string
			| CondOp of Condition*ConditionOp*Condition

	 and ConditionOp = EQ
	 		| GT
	 		| LT
	 		| GTEQ
	 		| LTEQ
	 		| AND
	 		| OR

fun   condOpToString EQ    = "=="
 	| condOpToString LT    = "<"
 	| condOpToString GT    = ">"
 	| condOpToString GTEQ  = ">="
  	| condOpToString LTEQ  = "<="
 	| condOpToString AND   = "&&"
 	| condOpToString OR    = "||"



datatype Statement = Ident of int * string
					 |FunCall of int * string
					 |Assignment of int * string * string
					 |Empty of int


(*SOME HELPER FUNCTIONS FOR CONSTRUCCTORS*)

fun eq    a b = CondOp(a,EQ,b)
fun lt    a b = CondOp(a,LT,b)
fun gt    a b = CondOp(a,GT,b)
fun nd    a b = CondOp(a,AND,b)
fun or    a b = CondOp(a,OR,b)



end
