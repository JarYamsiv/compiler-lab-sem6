(* The abstract syntax tree for expression *)
structure Ast = struct

 
datatype Expr  = Const of int
	       | Op    of Expr * BinOp * Expr
         

     and BinOp = Plus
	       | Minus
	       | Mul

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



datatype Statement = Id of string
          | As    of string * Expr
          | FnCl  of string

datatype CodeBlock =  Stlist of Statement list
					| Cblock of Condition*CodeBlock list
					| CEblock of Condition*CodeBlock list*CodeBlock list

datatype Function = Fun of string* CodeBlock list


datatype ProgramElement = St of Statement
						| Fn of Function 




fun binOpToString Plus  = "+"
  | binOpToString Minus = "-"
  | binOpToString Mul   = "*"

 fun  condOpToString EQ    = "=="
 	| condOpToString LT    = "<"
 	| condOpToString GT    = ">"
 	| condOpToString GTEQ  = ">="
  	| condOpToString LTEQ  = "<="
 	| condOpToString AND   = "&&"
 	| condOpToString OR    = "||"


fun plus  a b = Op (a, Plus, b)
fun minus a b = Op (a, Minus, b)
fun mul   a b = Op (a, Mul, b)

fun eq    a b = CondOp(a,EQ,b)
fun lt    a b = CondOp(a,LT,b)
fun gt    a b = CondOp(a,GT,b)
fun nd    a b = CondOp(a,AND,b)
fun or    a b = CondOp(a,OR,b)


end
