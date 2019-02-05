structure Translate =
struct

type RHS = Atom.atom list

val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"


structure StringKey =
struct
    type ord_key = RHS
    fun compare ((x::xs),(y::ys)) = (case Atom.compare (x,y) of
        								  GREATER => GREATER
        								| LESS	  => LESS
        								| EQUAL	  => compare (xs,ys))
    	|compare ((x::xs),[])	  = GREATER
    	|compare ([]	,(y::ys)) = LESS
    	|compare ([],	[])		  = EQUAL 

    fun convToRhs (x:ord_key) :RHS = x
end

structure ProductionSet = RedBlackSetFn(StringKey)




fun compileProdElem y :Atom.atom = case y of
				(Ast.St x) => (print((Atom.toString x)^" ");(x:Atom.atom))
				|(Ast.EPSILON) => (print("_");Atom.atom "_")
				|(Ast.EOP) => (print("$");Atom.atom "$") 



fun compileRHS (Ast.Rh y)  :RHS    = (map compileProdElem y)

fun compileRule (Ast.Rul(x,y)) = let 
									val _ = print (x^"->")
									val atom_val_x = (Atom.atom x)
									val prod_list = compileRHS y
									val tok_set = AtomSet.fromList(prod_list)
									val ret_list_set = ProductionSet.singleton(prod_list)

								in
								(	
									print ("\n");
									(atom_val_x,ret_list_set,tok_set)
									
								 )end


fun compile l rule_map sym_set tok_set= case l of
				(x::xs) =>	( let 
									val a = compileRule x
									val lhskey_compiled    = #1(a)
									val prodnlist_compiled = #2(a)
									val this_tok_table = #3(a)
									val this_map = AtomMap.singleton(lhskey_compiled,prodnlist_compiled)
									val this_sym_table = AtomSet.singleton(lhskey_compiled)									
									val (new_map,new_sym_table,new_tok_table) = compile xs rule_map sym_set tok_set

									val ret_sym_table = AtomSet.union(this_sym_table,new_sym_table)
									val ret_tok_table = AtomSet.union(new_tok_table,this_tok_table)
									val ret_tok_table = AtomSet.difference(ret_tok_table,new_sym_table)
									

								in 
									  ( AtomMap.unionWith (ProductionSet.union) (this_map,new_map) , ret_sym_table ,ret_tok_table) 
						    	end)
				| []	=> (AtomMap.empty,AtomSet.empty,AtomSet.empty)





(*fun print_map_elem (key,a) = (print ((Atom.toString key)^"->");(map (fn k=>print (" "^(Atom.toString k)) ) a);(print "\n")  )*)


fun printmap (rulemap,sym_table,tok_table)= 
	let
		fun print_symtok y = case AtomSet.member(sym_table,y) of
								true => (print (green^(Atom.toString y)^reset^" "))
								|false => (print (white^(Atom.toString y)^reset^" "))
		fun printProdn (x:RHS) = case x of
									(y::ys) => (print_symtok y;printProdn ys)
									| []	=>()

		fun print_prodns productions = 
			let
						val prodns_list = map StringKey.convToRhs (ProductionSet.listItems productions)
						fun   prnt_rhs_list ([x:RHS])	=(printProdn x)
				 			 |prnt_rhs_list []    = ()
				 			 |prnt_rhs_list ((x:RHS)::(xs:RHS list)) = ( printProdn x;print " | ";prnt_rhs_list xs  )
			in
				(prnt_rhs_list prodns_list)
			end
		fun print_map_elem (key,a) = (print (green^(Atom.toString key)^reset^" -> ");print_prodns a;(print "\n")  )


	in
		(AtomMap.appi print_map_elem rulemap )
	end




end
