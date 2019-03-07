structure Translate =
struct

val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"

fun addtabs n = if n <= 0 then
					("")
				else
					("   "^( addtabs (n-1)) )


fun compileExpr (Ast.Const x )         = (" "^(Int.toString x)^" ")
  | compileExpr (Ast.EVar  x )		   = (x)	
  | compileExpr (Ast.Op (x, oper, y))  = ((compileExpr x) ^ (Ast.binOpToString oper) ^ (compileExpr y ))



fun compileCondition (Ast.CConst x)	  = (" "^(Int.toString x)^" ")
  | compileCondition (Ast.CVar  x)		  = (" "^x^" ")
  | compileCondition (Ast.CondOp (x,oper,y)) = ((compileCondition x) ^ (Ast.condOpToString oper) ^ (compileCondition y))

fun   compileStatement (Ast.Id x)		t  =  ( (addtabs t) ^  (x^";\n")  )
	| compileStatement (Ast.As (x,exp))	t  =  ( (addtabs t) ^  (x^" = ") ^ (compileExpr exp) ^ (";\n") )
	| compileStatement (Ast.FnCl x)		t  =  ( (addtabs t) ^  (x^"();\n")  )

and  compileStatements  (t,(x :: xs))	  = ((compileStatement x t)^(compileStatements (t,xs)))
	|compileStatements  (t,[])	   		  = ("")

fun  compileBlock (Ast.Stlist slist)		t 		= (compileStatements (t,slist) )	
	|compileBlock (Ast.Cblock (x,clist))    t 		= (
														(addtabs t) ^ ("if(") ^ (compileCondition x) ^  ("){\n") ^
														(compileBlocks (t+1,clist) ) ^
														(addtabs t) ^ ("}\n")
													   )
	| compileBlock (Ast.CEblock (x,ilist,elist)) t  = (
														(addtabs t) ^  ("if(") ^ (compileCondition x) ^  ("){\n") ^

														(compileBlocks (t+1,ilist)) ^

														(addtabs t) ^ ("}\n") ^

														(addtabs t) ^  ("else{\n") ^

														(compileBlocks (t+1,elist) )^

														(addtabs t )^ ("}\n")
													  )

and  compileBlocks  (t,(x :: xs))				  = ((compileBlock x t)^(compileBlocks (t,xs))) 
	|compileBlocks  (t,[])				          = ("")

fun compileFun(Ast.Fun (x,g))		t  =  (
											("fun "^x^"(){\n")^
											(compileBlocks  (t+1,g) )^
											 ("}\n")
											 )



fun   compileElem (Ast.St statement)	  = compileStatement statement 0	
	| compileElem (Ast.Fn function)       = (compileFun  function 0) 







fun compile []        = ("")
  | compile (x :: xs) = ((compileElem x)^(compile xs))

end
