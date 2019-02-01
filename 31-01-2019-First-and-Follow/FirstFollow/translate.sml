structure Translate =
struct

structure Rule = struct
	type lhs = Atom.atom
	type rhs = Atom.atom list
end




fun prntRHS y = case y of
				(Ast.St x) => (print((Atom.toString x)^" ");x)
				|(Ast.EPSILON) => (print("_");Atom.atom "_")
				|(Ast.EOP) => (print("$");Atom.atom "$") 



fun compileRHS (Ast.Rh y)      = (map prntRHS y)

fun compileRule (Ast.Rul(x,y)) = let 
									val atom_val_x = (Atom.atom x)
									val ret_list = compileRHS y 
								in
								(	
									print ("<-"^x);
									print ("\n");
									(atom_val_x,ret_list)
									
								 )end


fun compile l map= case l of
				(x::xs) =>	(case AtomMap.find(map, #1(compileRule x) ) of
								   NONE =>       AtomMap.insert'( (compileRule x) , (compile xs map))
								 | SOME value => AtomMap.insert'( (compileRule x) , (compile xs map)) 
						    )
				| []	=> AtomMap.empty

(*fun print_single_rhs l = map ( fn k=>(print ((Atom.toString k)^" ") )  ) l

fun print_rhs set = AtomSet.app print_rhs set*)

fun print_map_elem (key,a) = (print ((Atom.toString key)^"->");(map (fn k=>print (" "^(Atom.toString k)) ) a);(print "\n")  )

fun printmap rulemap= (AtomMap.appi print_map_elem rulemap )

end
