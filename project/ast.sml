(* The abstract syntax tree for expression *)
structure Ast = struct
	datatype Argument = Arg of string*Atom.atom 
datatype Expr  =  Const of int
				| BVal of Bool
		   		| EVar of string 
		   		| ARVar of string*Expr
		   		| EFncl of string*Argument list
	       		| Op    of Expr * BinOp * Expr 
	       		| Erel of Expr* RelOp * Expr
	       		| Econd of Expr* ConditionOp * Expr
         

     and BinOp = Plus
	       | Minus
	       | Mul


	 and ConditionOp =  AND | OR

	 and RelOp = EQ | NEQ | LT | GT | GTEQ | LTEQ

	 and Bool = TRUE | FALSE

	 and Type = VOID | INT | BOOL | UNDEF 


	




	datatype Statement = EmptyStatement
			  | As    of string * Expr * Atom.atom * bool
	          | FnCl  of string * Argument list
	          | Ret of Expr
	          | If of Expr*Statement list
	          | IfEl of Expr*Statement list*Statement list
	          | StList of Statement list
	          | While of Expr*Statement list
	          | DirectC of string


	datatype Function = Fun of string* Statement list * Atom.atom * Argument list (*name , statements , type , arguments*)


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

	fun eq    a b = Erel(a,EQ,b)
	fun lt    a b = Erel(a,LT,b)
	fun gt    a b = Erel(a,GT,b)
	fun nd    a b = Econd(a,AND,b)
	fun or    a b = Econd(a,OR,b)


end

