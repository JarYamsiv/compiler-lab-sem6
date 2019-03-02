signature NEW_ORD_KEY = 
sig
	type ord_key;
	val compare: ord_key*ord_key-> order
	val empty:ord_key;
end

structure HelpFun = 
struct
	val red = "\u001b[31;1m"
	val green = "\u001b[32;1m"
	val white = "\u001b[37;1m"
	val yellow = "\u001b[33;1m"
	val grey = "\u001b[30;1m"
	val reset = "\u001b[0m"

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
											(SOME x,SOME y)   =>( compElemFn(x,y)=EQUAL andalso helper xs )
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

		fun printItemSet set= ItemSet.app printLr0Elem set

		fun printActionMap m = 
			let
				fun helper (k,(t,s)) = ( print ( (Atom.toString k)^" has action " );

					(case t of
					1 =>  (print (" goto "^(Int.toString s)^"\n" )  )
					|2 => (print (" shift to "^(Int.toString s)^"\n")  )
					|3 => (print " reduce \n")
					|_ => (print "\n")
					)
				)
			in
				AtomMap.appi helper m
			end

		type state_t = {num:int,set:ItemSet.set,aMap:(int*int) AtomMap.map}

		structure State:NEW_ORD_KEY = 
		struct
			type ord_key = state_t
			fun compare ((x:state_t),(y:state_t)) =ItemSet.compare((#set x),(#set y))
			val empty = {num=0,set=ItemSet.empty,aMap=AtomMap.empty}:state_t
		end

		structure StateSet = RedBlackSetFn(State)

		fun printState (s:state_t) = 
			let
				val _ = print (red^"\n\n=== "^(Int.toString(#num s))^" ===\n"^reset)
				val _ = printItemSet (#set s)
				val _ = printActionMap (#aMap s)
			in
				print (red^"======\n\n"^reset)
			end

		fun prntState (n,(s:state_t)) = 
			let
				val _ = print (red^"\n\n=== "^(Int.toString(n))^" ===\n"^reset)
				val _ = printItemSet (#set s)
				val _ = printActionMap (#aMap s)
			in
				print (red^"======\n\n"^reset)
			end



end



signature PROXY = sig
   type proxy_t
   type actual_t


   (*val proxy  : actual -> proxy
   val actual : proxy -> actual*)

   val makeItem:(int*HelpFun.ItemSet.set*(int*int) AtomMap.map)->HelpFun.state_t
   val addItem:actual_t -> unit
   val getCount:unit -> int
   val listItems:unit -> actual_t list

   val checkItem:actual_t -> bool
   val getItemId:actual_t -> proxy_t option
   val getItemIdEP:actual_t -> proxy_t
   val getItem : proxy_t -> actual_t option
   val getItemEP : proxy_t -> actual_t

   val headItem:unit -> actual_t option

   val lastInsertedItem:unit -> actual_t

   val correctReverseMap:unit->unit


end



functor Proxy (A:NEW_ORD_KEY):PROXY = 
struct
	type proxy_t = int
	type actual_t = A.ord_key

	structure Proxy_Map_key = 
	struct
		type ord_key = A.ord_key
		val compare = A.compare
	end

	structure Proxymap =  RedBlackMapFn(Proxy_Map_key)

	val count = ref 0
	val pMap = ref Proxymap.empty: (int Proxymap.map) ref
	val rMap = ref IntRedBlackMap.empty: (A.ord_key IntRedBlackMap.map) ref

	val lastItem = ref A.empty


	fun makeItem(n,s,a):HelpFun.state_t = {num=n,set=s,aMap=a}

	fun addNewItem (i:actual_t) = (	
								pMap := Proxymap.insert(!pMap,i,!count);
								rMap := IntRedBlackMap.insert(!rMap,!count,i) ;
								count := !count+1;
								lastItem := i
								);

	fun addToExistingItem (i:actual_t) = (
		pMap := Proxymap.insert(!pMap,i,!count)
		);

	fun addItem (i:actual_t) = 
		case Proxymap.find(!pMap,i) of
			SOME x=>(*addToExistingItem(i)*)()
			|NONE =>addNewItem(i)

	fun correctReverseMap () = 
		let
			val actual_list = Proxymap.listKeys(!pMap)
			val _ =map (fn k=> rMap := IntRedBlackMap.insert(!rMap,Proxymap.lookup(!pMap,k),k)) actual_list
		in
			()
		end

	fun getItemId (i:actual_t) = (Proxymap.find(!pMap,i))

	fun getItemIdEP (i:actual_t) = (Proxymap.lookup(!pMap,i))

	fun checkItem (i:actual_t) = (case Proxymap.find(!pMap,i) of SOME x=>true | NONE => false)

	fun getItem (p:proxy_t) = IntRedBlackMap.find(!rMap,p)

	fun getItemEP (p:proxy_t) = IntRedBlackMap.lookup(!rMap,p)

	fun getCount () = !count

	fun listItems () = Proxymap.listKeys(!pMap)

	fun headItem () = case Proxymap.listKeys(!pMap) of (x::xs) => SOME x | [] => NONE

	fun lastInsertedItem () = !lastItem

end

structure StateProxy = Proxy(HelpFun.State)

