structure HelpFun = 
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



	fun filter f (x::xs) =( case (f x) of
							SOME y=>[y]@(filter f xs)
							|NONE =>(filter f xs)
						)
	
		|filter f [] = []


end