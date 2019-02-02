structure Translate =
struct

type RHS = Atom.atom list



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
									val ret_list_set = ProductionSet.singleton(compileRHS y) 
								in
								(	
									print ("\n");
									(atom_val_x,ret_list_set)
									
								 )end


fun compile l rule_map= case l of
				(x::xs) =>	( let 
									val a = compileRule x
									val lhskey_compiled    = #1(a)
									val prodnlist_compiled = #2(a)
									val this_map = AtomMap.singleton(lhskey_compiled,prodnlist_compiled)

								in 
									AtomMap.unionWith (ProductionSet.union) (this_map,(compile xs rule_map))
						    	end)
				| []	=> AtomMap.empty





(*fun print_map_elem (key,a) = (print ((Atom.toString key)^"->");(map (fn k=>print (" "^(Atom.toString k)) ) a);(print "\n")  )*)

fun printProdnElem (x:RHS) = case x of
						(y::ys) => (print ((Atom.toString y)^" "); printProdnElem ys )
						| []	=> ()

fun  prnt_rhs_list ([x:RHS])	=(printProdnElem x)
	| prnt_rhs_list []    = ()
	| prnt_rhs_list ((x:RHS)::(xs:RHS list)) = ( printProdnElem x;print " | ";prnt_rhs_list xs  )



fun print_prodns productions = let
								   val prodns_list = map StringKey.convToRhs (ProductionSet.listItems productions)
								in
									(prnt_rhs_list prodns_list)
								end
fun print_map_elem (key,a) = (print ((Atom.toString key)^"-> ");print_prodns a;(print "\n")  )

fun printmap rulemap= (AtomMap.appi print_map_elem rulemap )

end
