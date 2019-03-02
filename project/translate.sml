structure Translate =
struct

val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"

fun addtab t = if t<=0 then () else (print "  "; addtab (t-1))

fun compile (x::xs) = (compileElem x;compile xs)
	|compile []		= ()

and
	compileElem (Ast.C_Statement st) = compileStatement st

and 
	compileStatement (Ast.C_St_Identifier (i,s)) = (addtab i;print (s^"\n"))
	|compileStatement (Ast.C_St_FunCall (i,s)) = (addtab i;print (s^"()\n"))

end
