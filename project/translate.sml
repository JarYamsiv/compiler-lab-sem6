structure Translate =
struct

val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"

fun addtabs n = if n <= 0 then
					(print "")
				else
					(print "   ";addtabs (n-1))


fun compileExpr (Ast.Const x)         = print (" "^(Int.toString x)^" ")
  | compileExpr (Ast.Op (x, oper, y)) = (compileExpr x ; print (Ast.binOpToString oper); compileExpr y )



fun compileCondition (Ast.CConst x)	  = print (" "^(Int.toString x)^" ")
  | compileCondition (Ast.CVar  x)		  = print (" "^x^" ")
  | compileCondition (Ast.CondOp (x,oper,y)) = (compileCondition x; print (Ast.condOpToString oper);compileCondition y)

fun   compileStatement (Ast.Id x)		t  =  (addtabs t; print (x^";\n"))
	| compileStatement (Ast.As (x,exp))	t  =  (addtabs t; print (x^" = ");compileExpr exp;print (";\n"))
	| compileStatement (Ast.FnCl x)		t  =  (addtabs t; print (x^"();\n"))

and  compileStatements  (t,(x :: xs))	  = (compileStatement x t;compileStatements (t,xs))
	|compileStatements  (t,[])	   		  = ()

fun  compileBlock (Ast.Stlist slist)		t 		= (compileStatements (t,slist) )	
	|compileBlock (Ast.Cblock (x,clist))    t 		= (
														addtabs t; print ("if("); compileCondition x; print ("){\n") ;
														compileBlocks (t+1,clist);
														addtabs t;print ("}\n")
													   )
	| compileBlock (Ast.CEblock (x,ilist,elist)) t  = (
														addtabs t; print ("if("); compileCondition x; print ("){\n") ;
														compileBlocks (t+1,ilist);
														addtabs t;print ("}\n");
														addtabs t; print ("else{\n") ;
														compileBlocks (t+1,elist);
														addtabs t;print ("}\n")
													  )

and  compileBlocks  (t,(x :: xs))				  = (compileBlock x t;compileBlocks (t,xs)) 
	|compileBlocks  (t,[])				          = ()

fun compileFun(Ast.Fun (x,g))		t  =  (
											print ("fun "^x^"(){\n");
											compileBlocks  (t+1,g) ;
											print ("}\n"))



fun   compileElem (Ast.St statement)	  = compileStatement statement 0	
	| compileElem (Ast.Fn function)       = (compileFun  function 0) 







fun compile []        = ()
  | compile (x :: xs) = (compileElem x; compile xs)

end
