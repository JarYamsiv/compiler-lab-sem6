structure Translate =
struct

fun compileExpr (Ast.Const x)         = print (" "^(Int.toString x)^" ")
  | compileExpr (Ast.Op (x, oper, y)) = (compileExpr x ; print (Ast.binOpToString oper); compileExpr y )
  | compileExpr (Ast.Id x)			  = print (x^"\n")
  | compileExpr (Ast.As (id,x))		  = (print (id^"=");compileExpr x)


fun compile []        = ()
  | compile (x :: xs) = (compileExpr x; compile xs)

end
