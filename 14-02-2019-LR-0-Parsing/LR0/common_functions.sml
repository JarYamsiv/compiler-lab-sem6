structure HelpFun = 
struct
	type RHS = Atom.atom list

	structure RHSKey =
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

	structure ProductionSet = RedBlackSetFn(RHSKey)


	fun id x =x

	fun filter f (x::xs) =( case (f x) of
							SOME y=>[y]@(filter f xs)
							|NONE =>(filter f xs)
						)
	
		|filter f [] = []

	type Productions_t = ProductionSet.set
	type Rules_t = Productions_t AtomMap.map
	type Grammar_t    = { sym_table : AtomSet.set, tok_table : AtomSet.set, rules : Rules_t }


end