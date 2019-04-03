(* The abstract syntax tree for expression *)
structure Ast = struct

 
datatype Expr  =  Const of int
		   		| EVar of string 
		   		| ARVar of string*Expr
	       		| Op    of Expr * BinOp * Expr
         

     and BinOp = Plus
	       | Minus
	       | Mul

	 and Condition = BConst of Bool
			| CondOp of Condition*ConditionOp*Condition
			| Rel of Expr* RelOp * Expr

	 and ConditionOp =  AND | OR

	 and RelOp = EQ | NEQ | LT | GT | GTEQ | LTEQ

	 and Bool = TRUE | FALSE

datatype Type = VOID | INT | BOOL | UNDEF



datatype Statement = EmptyStatement
		  | As    of string * Expr * Type * bool
		  | BAs   of string * Condition * bool
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

fun  relOpToString EQ     = "=="
	|relOpToString NEQ    = "!="
	|relOpToString LT     = "<"
	|relOpToString GT     = ">"
	|relOpToString LTEQ   = "<="
	|relOpToString GTEQ   = ">="


fun processExpr (x,oper,y) = case oper of
								Plus => (x+y)
								|Minus => (x-y)
								|Mul => (x*y)

fun plus  a b = Op (a, Plus, b)
fun minus a b = Op (a, Minus, b)
fun mul   a b = Op (a, Mul, b)

fun eq    a b = Rel(a,EQ,b)
fun lt    a b = Rel(a,LT,b)
fun gt    a b = Rel(a,GT,b)
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

	 and Condition = BConst of Bool
			| CondOp of Condition*ConditionOp*Condition
			| Rel of Expr* RelOp * Expr

	 and ConditionOp =  AND | OR

	 and RelOp = EQ | NEQ | LT | GT | GTEQ | LTEQ

	 and Bool = TRUE | FALSE

	datatype Type = VOID | INT | BOOL | UNDEF



	datatype Statement = EmptyStatement
			  | As    of string * Expr * Type * bool
			  | BAs   of string * Condition * bool
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

end

fun  oper_conv (Ast.Plus) = (CAst.Plus)
	|oper_conv (Ast.Minus) = (CAst.Minus)
	|oper_conv (Ast.Mul) = (CAst.Mul)

fun   relOp_conv (Ast.EQ) = (CAst.EQ)
	| relOp_conv (Ast.GT) = (CAst.GT)
	| relOp_conv (Ast.LT) = (CAst.LT)
	| relOp_conv (Ast.GTEQ) = (CAst.GTEQ)
	| relOp_conv (Ast.LTEQ) = (CAst.LTEQ)
	| relOp_conv (Ast.NEQ) = (CAst.NEQ)

fun   condOp_conv (Ast.AND) = (CAst.AND)
	| condOp_conv (Ast.OR) = (CAst.OR)
