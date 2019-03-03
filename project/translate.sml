structure Translate =
struct

fun addtab t = if t<=0 then () else (print "  "; addtab (t-1))

val lastI = ref 0



fun Ind i =
	let
	 	val last = !lastI
	 	val cur = i
	 	val _ = lastI:=i
	 in
	 	case Int.compare(cur,last) of
	 		EQUAL => (addtab cur)
	 		|LESS => (addtab cur;print "} \n"; addtab cur)
	 		|GREATER => (addtab cur;print "{ \n"; addtab cur)
	 end 

val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"



fun compile s_list = map compileStatement s_list



and 
	compileStatement (Ast.Ident (i,s)) = (  Ind i ;print (s^"\n")    )
	|compileStatement (Ast.FunCall (i,s)) = (   Ind i;print (s^"()\n")    )
	|compileStatement (Ast.Assignment (i,s1,s2)) = ( Ind i;print (s1^"="^s2^"\n")  )
	|compileStatement (Ast.Empty i ) = (Ind i;print "@@\n")

end
