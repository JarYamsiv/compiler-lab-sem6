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


fun compileExpr (Ast.Const x)         = print (" "^yellow^(Int.toString x)^reset^" ")
  | compileExpr (Ast.Op (x, oper, y)) = (compileExpr x ; print (Ast.binOpToString oper); compileExpr y )



fun compileCondition (Ast.CConst x)	  = print (" "^yellow^(Int.toString x)^reset^" ")
  | compileCondition (Ast.CVar  x)		  = print (" "^white^x^reset^" ")
  | compileCondition (Ast.CondOp (x,oper,y)) = (compileCondition x; print (Ast.condOpToString oper);compileCondition y)

fun   compileStatement (Ast.Id x)		t  =  (addtabs t; print (white^x^green^";\n"^reset))
	| compileStatement (Ast.As (x,exp))	t  =  (addtabs t; print (white^x^green^" = "^reset);compileExpr exp;print (green^";\n"^reset))
	| compileStatement (Ast.FnCl x)		t  =  (addtabs t; print (white^x^green^"();\n"^reset))

and  compileStatements  (t,(x :: xs))	  = (compileStatement x t;compileStatements (t,xs))
	|compileStatements  (t,[])	   		  = ()

fun  compileBlock (Ast.Stlist slist)		t 		= (compileStatements (t,slist) )	
	|compileBlock (Ast.Cblock (x,clist))    t 		= (
														addtabs t; print (red^"if"^green^"("^reset); compileCondition x; print (green^"){\n"^reset) ;
														compileBlocks (t+1,clist);
														addtabs t;print (green^"}\n"^reset)
													   )
	| compileBlock (Ast.CEblock (x,ilist,elist)) t  = (
														addtabs t; print (red^"if"^green^"("^reset); compileCondition x; print (green^"){\n"^reset) ;
														compileBlocks (t+1,ilist);
														addtabs t;print (green^"}\n"^reset);
														addtabs t; print (red^"else"^green^"{\n"^reset) ;
														compileBlocks (t+1,elist);
														addtabs t;print (green^"}\n"^reset)
													  )

and  compileBlocks  (t,(x :: xs))				  = (compileBlock x t;compileBlocks (t,xs)) 
	|compileBlocks  (t,[])				          = ()

fun compileFun(Ast.Fun (x,g))		t  =  (
											print (red^"fun "^white^x^green^"(){\n"^reset);
											compileBlocks  (t+1,g) ;
											print (green^"}\n"^reset))



fun   compileElem (Ast.St statement)	  = compileStatement statement 0	
	| compileElem (Ast.Fn function)       = (compileFun  function 0) 







fun compile []        = ()
  | compile (x :: xs) = (compileElem x; compile xs)

end
