structure HelpFun = 
struct

	fun id x =x

	fun filter f (x::xs) =( case (f x) of
							SOME y=>[y]@(filter f xs)
							|NONE =>(filter f xs)
						)
	
		|filter f [] = []

	fun forall f (x::xs) = (f x) andalso (forall f xs)
		|forall f []		 = true

	fun there_exists f (x::xs) = if (f x) then true else there_exists f xs
		|there_exists f []	   = false

	fun array_redn (x::xs) = x@(array_redn xs) 
		|array_redn [] = []

	fun bool_to_op f x = if (f x) then (SOME x) else (NONE)





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


	

	type Productions_t = ProductionSet.set
	type Rules_t = Productions_t AtomMap.map
	type Grammar_t    = { sym_table : AtomSet.set, tok_table : AtomSet.set, rules : Rules_t ,starting_sym: Atom.atom}




	fun ret_prod_list (rulemap,lhs):RHS list = map RHSKey.convToRhs (ProductionSet.listItems (valOf  (AtomMap.find (rulemap,lhs))))

	fun PrintSet (set:AtomSet.set):unit =
		 (
		 	print "{ ";
		(map ( fn k=>print((Atom.toString k)^" ")  ) (AtomSet.listItems(set)) ); 
		print " }\n"
			)





	fun forall_symbols_g f (gram:Grammar_t)=map f (AtomSet.listItems(#sym_table gram))

		
		

	fun map_lhs_productions (gram:Grammar_t) (f:Atom.atom*RHS -> 'a) :'a list=
	let
		val symbols =AtomSet.listItems(#sym_table gram)

		fun for_prodns_of_lhs (lhs:Atom.atom):'a list=
			let
				val prod_list = ret_prod_list ((#rules gram),lhs)
				val lhs_prod = map (fn k=>(lhs,k)) prod_list
			in
				map f lhs_prod
			end
	in
		array_redn (map for_prodns_of_lhs symbols)
	end

	fun MakeFixedPointFn (update) (compare) =
		let
			fun fp_helper cur=
				case compare(update cur,cur) of
					true=>update cur
					|false => fp_helper (update cur)
		in
			fp_helper
		end

	fun MakeMapCompareFn (listKeyFn) (compKeyFn) (findFn) (compElemFn) = 
		let
			fun eq (m1,m2)=
				let
					val m1_keys = listKeyFn(m1)
					val m2_keys = listKeyFn(m2)

					fun keys_compare ((x::xs),(y::ys)) = compKeyFn(x,y) andalso keys_compare (xs,ys)
						|keys_compare ([],[])		   = true
						|keys_compare (_,_)			   = false

					fun helper (x::xs) = (
											case ( findFn(m1,x) , findFn(m2,x) ) of
											(SOME x,SOME y) => helper xs
											|_				=>false
										 )
						|helper []	   = true
				in
					case keys_compare(m1_keys,m2_keys) of
						true => helper (m1_keys)
						|false=> false
				end
		in
			eq
		end

		fun compare_list ((x::xs),(y::ys)) = (case Atom.compare (x,y) of
	        								  GREATER => GREATER
	        								| LESS	  => LESS
	        								| EQUAL	  => compare_list (xs,ys))
	    	|compare_list ((x::xs),[])	  = GREATER
	    	|compare_list ([]	,(y::ys)) = LESS
	    	|compare_list ([],	[])		  = EQUAL

	    type lr0i_t= {lhs:Atom.atom,bef:(Atom.atom list),aft:(Atom.atom list)}

	    fun compare_lr0 ((x:lr0i_t),(y:lr0i_t)) = 
	    	let
	    		val xlhs = #lhs x
	    		val xbef = #bef x
	    		val xaft = #aft x

	    		val ylhs = #lhs y
	    		val ybef = #bef y
	    		val yaft = #aft y
	    	in
	    		case Atom.compare(xlhs,ylhs) of
	    			EQUAL => (case compare_list(xbef,ybef) of
	    						EQUAL => compare_list(xaft,yaft)
	    						|y 	  => y
	    				)
	    			|x 	  => x
	    	end


		structure LR0_ITEM_KEY = 
		struct
			type ord_key= lr0i_t
			val compare = compare_lr0
		end

		structure ItemSet = RedBlackSetFn(LR0_ITEM_KEY)

		fun printProdn (p:Atom.atom list) = map (fn k=> print((Atom.toString k)^" ")) p

		fun printLr0Elem (e:lr0i_t) = ( (print o Atom.toString) (#lhs e);
										print "-> [";
										printProdn (#bef e);print ".";printProdn (#aft e);(print "]\n"))

		fun printItemSet set=ItemSet.app printLr0Elem set

		fun printActionMap m = 
			let
				fun helper (k,t) = ( print ( (Atom.toString k)^" has action " );

					(case t of
					1 =>  (print " goto \n")
					|2 => (print " shift \n")
					|3 => (print " reduce \n")
					|_ => (print "\n")
					)
				)
			in
				AtomMap.appi helper m
			end



end


signature FIXED_POINT_KEY = 
sig
	type t
	val update:t->t
	val compare:t*t->bool
end

functor FP_Constructor (S:FIXED_POINT_KEY) = 
struct
	type t = S.t
	fun fixed_point (start_point:t)=
		let
			val (next:t) = S.update(start_point)
		in
			case S.compare(next,start_point) of
				true => next
				|false => fixed_point(next)
		end
end




signature MAP_EQ_SIG = 
sig
	type t
	val compare: t*t -> bool
	structure map:ORD_MAP
end

functor MakeMapEqualityStruct (S:MAP_EQ_SIG) = 
struct
	type tp=S.t
	fun eq (map1,map2) = 
		let
			val key_list = S.map.listKeys(map1)
			fun helper (x::xs) =
				(case (S.map.find(map1,x),S.map.find(map2,x)) of
					(SOME (v1:tp),SOME (v2:tp)) => S.compare(v1,v2) andalso helper xs
					|(NONE,NONE)	=> helper xs
					|_				=> false
				)
				|helper [] = true
		in
			helper key_list
		end
end