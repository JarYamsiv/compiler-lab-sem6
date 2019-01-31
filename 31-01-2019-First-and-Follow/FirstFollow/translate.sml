structure Translate =
struct

val symbol_set =ref AtomSet.empty



fun prntLHS (Ast.St x)		   = (print (x^" "))
	|prntLHS (Ast.EPSILON)     = (print ("_"))

fun singleLHS  (x::xs)         = (prntLHS x;singleLHS xs)
	| singleLHS [] 			   = ()

fun compileLHS (Ast.Lh y)      = (singleLHS y)

fun compileRule (Ast.Rul(x,y)) = let val atom_val_x = (Atom.atom x) in
								(	

									print (x^"->"); symbol_set:= AtomSet.add( !symbol_set ,atom_val_x );
									compileLHS y;
									print ("\n")
								 )end



fun compile []        = (AtomSet.app (print o Atom.toString) (!symbol_set)  )
  | compile (x :: xs) = (compileRule x; compile xs)

end
