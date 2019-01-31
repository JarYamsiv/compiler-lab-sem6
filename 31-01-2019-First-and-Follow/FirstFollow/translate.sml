structure Translate =
struct

fun singleLHS  (x::xs)         = (print (x^" ");singleLHS xs)
	| singleLHS [] 			   = ()

fun compileLHS (Ast.Lh y)      = (singleLHS y)

fun compileRule (Ast.Rul(x,y)) = (print (x^"->"); compileLHS y;print ("\n"))



fun compile []        = ()
  | compile (x :: xs) = (compileRule x; compile xs)

end
