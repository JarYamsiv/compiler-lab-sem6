(* The abstract syntax tree for expression *)
structure Ast = struct

 
datatype Expr  =  Const of int
		   		| EVar of string 
		   		| ARVar of string*Expr
	       		| Op    of Expr * BinOp * Expr
         

     and BinOp = Plus
	       | Minus
	       | Mul

datatype Condition = BConst of Bool
			| CondOp of Condition*ConditionOp*Condition

	 and ConditionOp =  AND | OR

	 and RelOp = EQ | NEQ | LT | GT | GTEQ | LTEQ

	 and Bool = TRUE | FALSE

datatype Type = VOID | INT | BOOL | UNDEF



datatype Statement = EmptyStatement
		  | As    of string * Expr * Type * bool
          | FnCl  of string
          | Ret of Expr
          | If of Condition*Statement list
          | IfEl of Condition*Statement list*Statement list
          | StList of Statement list
          | While of Condition*Statement list
          | DirectC of string


datatype Function = Fun of string* Statement list * Type


datatype ProgramElement = St of Statement
						| Fn of Function 




fun binOpToString Plus  = "+"
  | binOpToString Minus = "-"
  | binOpToString Mul   = "*"

fun  condOpToString AND   = "&&"
 	| condOpToString OR    = "||"

fun processExpr (x,oper,y) = case oper of
								Plus => (x+y)
								|Minus => (x-y)
								|Mul => (x*y)

fun plus  a b = Op (a, Plus, b)
fun minus a b = Op (a, Minus, b)
fun mul   a b = Op (a, Mul, b)

(*fun eq    a b = CondOp(a,EQ,b)
fun lt    a b = CondOp(a,LT,b)
fun gt    a b = CondOp(a,GT,b)*)
fun nd    a b = CondOp(a,AND,b)
fun or    a b = CondOp(a,OR,b)


end

structure CAst = 
struct
	datatype Expr  =  Const of int
		   		| EVar of string 
		   		| ARVar of string*Expr
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

	datatype Type = VOID | INT 



	datatype Statement = As    of string * Expr * Type * bool
	          | FnCl  of string
	          | Ret of Expr
	          | If of Condition*Statement list
	          | IfEl of Condition*Statement list*Statement list



	datatype Function = Fun of string* Statement list * Type


	datatype ProgramElement = St of Statement
							| Fn of Function 
end

(*fun  oper_conv (Ast.Plus) = (CAst.Plus)
	|oper_conv (Ast.Minus) = (CAst.Minus)
	|oper_conv (Ast.Mul) = (CAst.Mul)

fun   condOp_conv (Ast.EQ) = (CAst.EQ)
	| condOp_conv (Ast.GT) = (CAst.GT)
	| condOp_conv (Ast.LT) = (CAst.LT)
	| condOp_conv (Ast.GTEQ) = (CAst.GTEQ)
	| condOp_conv (Ast.LTEQ) = (CAst.LTEQ)
	| condOp_conv (Ast.AND) = (CAst.AND)
	| condOp_conv (Ast.OR) = (CAst.OR)*)
