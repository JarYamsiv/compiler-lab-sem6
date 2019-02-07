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



(*fun printmap (rulemap,sym_table,tok_table)= 
	let
		fun print_symtok y = case AtomSet.member(sym_table,y) of
								true => (print (green^(Atom.toString y)^reset^" "))
								|false => (print (red^(Atom.toString y)^reset^" "))
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
	end*)

fun printmap (rulemap,sym_table,tok_table)=
	let
		val a =10
		fun print_prodns rhs_from_map=
					let
						val rhs_from_map_e =valOf rhs_from_map
						val prodn_list = map StringKey.convToRhs (ProductionSet.listItems rhs_from_map_e)

						fun print_symtok x = case AtomSet.member(sym_table,x) of
							true => (print (green^(Atom.toString x)^reset^" "))
							|false => (print (red^(Atom.toString x)^reset^" "))

						fun  prnt_rhs_list [x] = (map print_symtok x;())
							|prnt_rhs_list []	=()
							|prnt_rhs_list (x::xs)=(map print_symtok x;print " | ";prnt_rhs_list xs)
					in
						(prnt_rhs_list prodn_list)
					end
		fun print_rule symbol = (print (green^(Atom.toString symbol)^reset^"->");print_prodns (AtomMap.find(rulemap,symbol));print "\n")
	in
		map print_rule (AtomSet.listItems sym_table)
	end

fun ret_prod_list (rulemap,lhs):RHS list = map StringKey.convToRhs (ProductionSet.listItems (valOf  (AtomMap.find (rulemap,lhs))))


fun filter f (x::xs) = (case (f x) of
	SOME y=>[y]@(filter f xs)
	|NONE =>(filter f xs)
	)
|filter f [] = []


fun calc_nullable (rulemap,sym_table,tok_table) = let

	fun base_nullable lhs = 
		let
			val (prodn_list:RHS list) = ret_prod_list (rulemap,lhs)

			fun isEpsilon [x:Atom.atom] = if Atom.compare(x,Atom.atom "_") = EQUAL then SOME x else NONE
				|isEpsilon _	 = NONE
				
			val isEpsilon_productions = filter isEpsilon prodn_list

			val _ = print ((Atom.toString lhs)^"->")
		in
			case isEpsilon_productions of
				[] => (print "no first epsilon\n";NONE)
				|_ => (print "first epsilon\n";SOME lhs)
		end

	val base_nullable_set =AtomSet.fromList (filter base_nullable (AtomSet.listItems(sym_table)))

	fun next_nullable_set cur_set =
		let
			fun singularProdn lhs = 
				let
					val prod_list = ret_prod_list (rulemap,lhs)
					val sing_prodn = filter (fn k=>if length(k)>1 then NONE else
						(case AtomSet.member(cur_set,(hd k)) of
													true => SOME k
													|false =>NONE)
						)
					prod_list
				in
					case sing_prodn of
						[] =>(NONE)
						|_ =>(SOME lhs)
				end
			val next_set = filter singularProdn (AtomSet.listItems sym_table)
		in
			AtomSet.fromList(next_set)
		end

	fun recursive_nullable base_set = 
		let
			val next_set = AtomSet.union(base_set,(next_nullable_set base_set))

		in
			case AtomSet.equal(next_set,base_set) of
				 true  => next_set
				|false => (recursive_nullable next_set)
		end

		val nullable_set = recursive_nullable base_nullable_set
in
	map (fn k=> print(green^(Atom.toString k)^reset^"\n")) (AtomSet.listItems (nullable_set));nullable_set
end




end

(*
	Type definitions:

	Map : 
		Key : Atom.atom
		Element: ProductionSet.set


*)
