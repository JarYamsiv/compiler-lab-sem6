structure Translate =
struct


fun compileExpr (Ast.Const x)         = print (" "^(Int.toString x)^" ")
  | compileExpr (Ast.Op (x, oper, y)) = (compileExpr x ; print (Ast.binOpToString oper); compileExpr y )

fun   compileStatement (Ast.Id x)			  =    print (x^"\n")
	| compileStatement (Ast.As (x,exp))	  	  =  ( print (x^" = ");compileExpr exp;print "\n")

fun   compileElem (Ast.St state)	  	  = compileStatement(state)	
	| compileElem _  					  = () 







fun compile []        = ()
  | compile (x :: xs) = (compileElem x; compile xs)

end
