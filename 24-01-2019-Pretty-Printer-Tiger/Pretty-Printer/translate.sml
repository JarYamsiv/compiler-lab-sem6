structure Translate =
struct

fun addtabs n = if n <= 0 then
					(print "")
				else
					(print "\t";addtabs (n-1))


fun compileExpr (Ast.Const x)         = print (" "^(Int.toString x)^" ")
  | compileExpr (Ast.Op (x, oper, y)) = (compileExpr x ; print (Ast.binOpToString oper); compileExpr y )


fun   compileStatement (Ast.Id x)		t  =  (addtabs t; print (x^"\n"))
	| compileStatement (Ast.As (x,exp))	t  =  (addtabs t; print (x^" = ");compileExpr exp;print "\n")

fun  compileStatements  (t,((x:Ast.Statement) :: (xs:Ast.Statement list)))	  = (compileStatement x t;compileStatements (t,xs))
	|compileStatements  (t,[])	   											  = ()

fun compileBlock (Ast.Stlist slist)		t  = (compileStatements (t,slist) )

fun  compileBlocks  (t,((x:Ast.CodeBlock) :: (xs:Ast.CodeBlock list)))				  = (compileBlock x t;compileBlocks (t,xs)) 
	|compileBlocks  (t,[])				         									  = ()

fun compileFun(Ast.Fun (x,g))		t  =  (print ("fun "^x^"(){\n");compileBlocks  (t+1,g) ;print "}\n")



fun   compileElem (Ast.St state)	  	  = compileStatement state 0	
	| compileElem (Ast.Fn function)       = (compileFun  function 0) 
	| compileElem (Ast.Cd codeblock)	  = (compileBlock codeblock 0)







fun compile []        = ()
  | compile (x :: xs) = (compileElem x; compile xs)

end
