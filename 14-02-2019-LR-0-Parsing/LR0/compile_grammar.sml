structure CompGram = 
struct
	val red = "\u001b[31;1m"
	val green = "\u001b[32;1m"
	val white = "\u001b[37;1m"
	val yellow = "\u001b[33;1m"
	val grey = "\u001b[30;1m"
	val reset = "\u001b[0m"
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
	fun ret_prod_list (rulemap,lhs):RHS list = map StringKey.convToRhs (ProductionSet.listItems (valOf  (AtomMap.find (rulemap,lhs))))


	fun filter f (x::xs) = (case (f x) of
		SOME y=>[y]@(filter f xs)
		|NONE =>(filter f xs)
		)
	|filter f [] = []
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*======================================================CALCULATION OF NULLABLE=====================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
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
					fun string_nullable (s::tring) = AtomSet.member(cur_set,s) andalso string_nullable tring
						|string_nullable []		   = true

					val prod_list = ret_prod_list (rulemap,lhs)
					val sing_prodn = filter (fn k=>case (string_nullable k) of
													true =>SOME k
													|false =>NONE
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

(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*======================================================CALCULATION OF FIRST========================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)



fun calc_first (rulemap,sym_table,tok_table,nullable_set)=
	let

		fun base_first lhs=
			let
				val prod_list = ret_prod_list (rulemap,lhs)
			in
				AtomSet.fromList( filter (
					fn k=>case AtomSet.member(tok_table,(hd k)) of
						 true  => SOME (hd k)
						|false => NONE
					) prod_list
				)
			end

		fun calc_base_map (x::xs) = 
			let
				val this_map = AtomMap.singleton(x,base_first x)
				val new_map = calc_base_map xs
			in
				AtomMap.unionWith (AtomSet.union) (this_map,new_map)
			end
			| calc_base_map [] = AtomMap.empty

		val base_map = calc_base_map (AtomSet.listItems sym_table)
 

		fun lhs_update lhs cur_map=
			let
				val prod_list = ret_prod_list (rulemap,lhs)

				fun first_of_current_prodn (symtok::symtok_ls) = 
					(
					case AtomSet.member(sym_table,symtok) of
						false   =>(case Atom.compare (symtok ,Atom.atom "_") of
							EQUAL =>AtomSet.empty
							|_ =>AtomSet.singleton(symtok)
							)
						|true =>(
						let 
								val this_symtok_first = (case AtomMap.find(cur_map,symtok) of
														SOME x=>(x)
														|NONE =>(AtomSet.empty))
							in
							case AtomSet.member(nullable_set,symtok) of
								false =>(
										this_symtok_first
									)
								|true =>(
										AtomSet.union(this_symtok_first,first_of_current_prodn symtok_ls)
									)
							end
							)
					)
					|first_of_current_prodn [] = AtomSet.empty
				val new_first_of_prod_set_list = map first_of_current_prodn prod_list
				val new_lhs_first_set = foldl (AtomSet.union) (AtomSet.empty) new_first_of_prod_set_list
				val sing_map = AtomMap.singleton(lhs,new_lhs_first_set)
				val union_map = AtomMap.unionWith (AtomSet.union) (sing_map,cur_map)
				(*val _ = print "pass-\n"
				val _ = AtomMap.appi (fn (key,first_set)=>  (print ((Atom.toString key)^" :{"); (printSet first_set);print "}\n")  ) union_map
				val _ = print "\n"*)

			in
				union_map
			end

		fun calc_next_map (x::xs) this_map = 
			let
				val x_updated_map = lhs_update x this_map
				val new_map = calc_next_map xs x_updated_map
			in
				AtomMap.unionWith (AtomSet.union) (new_map,x_updated_map)
			end
			|calc_next_map [] this_map= (this_map)

		fun recursive_first base_map = 
			let
				val next_map = AtomMap.unionWith (AtomSet.union) (base_map,(calc_next_map (AtomSet.listItems sym_table) base_map))
				fun map_equality map1 map2 (k::ks) = 
					let
					 	val m1_val = AtomMap.find(map1,k)
					 	val m2_val = AtomMap.find(map2,k)
					 in
					 	case (m1_val,m2_val) of
					 		(SOME v1,SOME v2) 	  =>AtomSet.equal(v1,v2) andalso (map_equality map1 map2 ks)
					 		|(NONE,NONE)		  =>true andalso (map_equality map1 map2 ks)
					 		|_					  =>false
					 end
					 |map_equality map1 map2 [] = true
			in
				case map_equality base_map next_map (AtomSet.listItems sym_table) of
					true   => next_map
					|false => recursive_first next_map
			end

		val final_map = recursive_first base_map

		fun printSet set= map (fn k=>print((Atom.toString k) ^ " ")) (AtomSet.listItems set)

	in
		(*AtomMap.appi (fn (key,first_set) => (print((Atom.toString key)^": ");printSet first_set  ; print "\n" )) base_map*)
		print "first elements map:\n";
		map (fn k=>(print ((Atom.toString k)^"->");
						(
							case AtomMap.find(final_map,k) of
								SOME x=>printSet x
								|NONE =>[()]
						);
						print ("\n")
					)
			) (AtomSet.listItems sym_table);final_map
	end

(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*======================================================CALCULATION OF FOLLOW=======================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)


	fun calc_follow(rulemap,sym_table,tok_table,nullable_set,first_map)=
		let
			val lhs_list = AtomSet.listItems(sym_table)

			fun string_first (symtok::symtok_ls) f_map= 
					(
					case AtomSet.member(sym_table,symtok) of
						false   =>(case Atom.compare (symtok ,Atom.atom "_") of
							EQUAL =>(string_first symtok_ls f_map)
							|_ =>AtomSet.singleton(symtok)
							)
						|true =>(
						let 
								val this_symtok_first = (case AtomMap.find(f_map,symtok) of
														SOME x=>(x)
														|NONE =>(AtomSet.empty))
							in
							case AtomSet.member(nullable_set,symtok) of
								false =>(
										this_symtok_first
									)
								|true =>(
										AtomSet.union(this_symtok_first,string_first symtok_ls f_map)
									)
							end
							)
					)
				|string_first [] f_map	 = (AtomSet.empty)

			fun current_follow lhs a cur_flw_map= 
				let
					val prod_list = ret_prod_list(rulemap,lhs)
					fun nullable_string (x::xs) = AtomSet.member(nullable_set,x) andalso nullable_string xs
						|nullable_string []		= true

					val prodn_list = ret_prod_list(rulemap,lhs)
					fun presence x (y::ys) = (case Atom.compare(x,y) of
												EQUAL => true
												|_ => presence x ys)
						| presence x []    = false

					fun substringer x (y::ys) = (case Atom.compare(x,y) of
												EQUAL => ys
												|_	  => (substringer x ys)	)
						|substringer x []	  = []

					fun flw prodn = 
						let
							val string_after_a = substringer a prodn
						in
							case (presence a prodn) of
							true=> (case (nullable_string string_after_a) of 
										false  => (string_first string_after_a first_map)
										| true => (case AtomMap.find(cur_flw_map,lhs) of
													SOME x => AtomSet.union(x,(string_first string_after_a first_map))
													|NONE => string_first string_after_a first_map
									)
							)
							|false => (AtomSet.empty)
						end
					val next_follow_set_list = map flw prod_list
					val next_follow_set = foldl (AtomSet.union) (AtomSet.empty) next_follow_set_list
				in
					AtomMap.unionWith (AtomSet.union) (cur_flw_map,(AtomMap.singleton(a,next_follow_set)))
				end

			fun rec_follow cur_map = 
				let
					val symbols = AtomSet.listItems(sym_table)
					fun map_over_lhs a = 
						let
							val map_list = map (fn k=>(current_follow k a cur_map)) symbols
							val new_map = foldl ( fn (x,y)=>(AtomMap.unionWith AtomSet.union (x,y))  ) (AtomMap.empty) map_list
						in
							new_map
						end
					val next_rec_map_list = map map_over_lhs symbols
					val next_rec_map = foldl ( fn (x,y)=>(AtomMap.unionWith AtomSet.union (x,y))  ) (AtomMap.empty) next_rec_map_list
					fun map_equality map1 map2 (k::ks) = 
					let
					 	val m1_val = AtomMap.find(map1,k)
					 	val m2_val = AtomMap.find(map2,k)
					 in
					 	case (m1_val,m2_val) of
					 		(SOME v1,SOME v2) 	  =>AtomSet.equal(v1,v2) andalso (map_equality map1 map2 ks)
					 		|(NONE,NONE)		  =>true andalso (map_equality map1 map2 ks)
					 		|_					  =>false
					 end
					 |map_equality map1 map2 [] = true
				in
					case map_equality cur_map next_rec_map symbols of
						true => (cur_map)
						|false => (rec_follow next_rec_map)
				end

			val first_map_final = rec_follow AtomMap.empty

			fun printSet set= map (fn k=>print((Atom.toString k) ^ " ")) (AtomSet.listItems set)
		in
			print "follow elements map:\n";
			map (fn k=>(print ((Atom.toString k)^"->");
						(
							case AtomMap.find(first_map_final,k) of
								SOME x=>printSet x
								|NONE =>[()]
						);
						print ("\n")
					)
			) lhs_list;first_map_final
		end

(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*===================================================CALCULATION OF LR(0) TABLE=====================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)

structure LR0_key =
struct
    type ord_key = (Atom.atom*Atom.atom list)

    fun compare_list ((x::xs),(y::ys)) = (case Atom.compare (x,y) of
        								  GREATER => GREATER
        								| LESS	  => LESS
        								| EQUAL	  => compare_list (xs,ys))
    	|compare_list ((x::xs),[])	  = GREATER
    	|compare_list ([]	,(y::ys)) = LESS
    	|compare_list ([],	[])		  = EQUAL

    fun compare ( (x_lhs,x_list) , (y_lhs,y_list) ) = 
    	case Atom.compare (x_lhs,y_lhs) of
    		EQUAL => compare_list (x_list,y_list)
    		| x => x

    fun convToRhs (x:ord_key) :Atom.atom*Atom.atom list = x
end

structure LR0_itemSet = RedBlackSetFn(LR0_key)

type state_t = int*LR0_itemSet.set


fun print_production prodn = (
									print "[";
									map (fn k=>print (  (Atom.toString k)^" "  )) prodn;
									print "]"
								)


fun calc_lr0 (
	rulemap,
	sym_table,
	tok_table,
	nullable_set,
	first_map,
	follow_map,
	starting_symbol
	)=
	let
		fun split_on_dot ((x::xs),(y::ys) )= (
												case Atom.compare(y,Atom.atom ".") of
												EQUAL => (  ((x::xs)) , ys  )
												|_	  => split_on_dot (  ((x::xs)@[y]) , ys  )
											   )
			|split_on_dot ([] , (y::ys)) = (
											case Atom.compare(y,Atom.atom ".") of
											EQUAL => ( [],ys)
											|_	  => split_on_dot ([y],ys)
										   )

			|split_on_dot ((x::xs),[])			 = ([],[])
			|split_on_dot ([],[])			 = ([],[])

		



		fun after_dot (x::xs) = (case Atom.compare(x,Atom.atom ".") of
									EQUAL => (case xs of
												(x::xs) => SOME x
												|[]		=> NONE
											 )
									|_ =>(after_dot xs))
			| after_dot []	  = NONE

		fun create_action_map (state_num,lr0_set,action_map) = 
			let
				val lr0_list = LR0_itemSet.listItems(lr0_set)
				val after_dot_list = map  (fn (lhs,production) =>  (after_dot production) )  (lr0_list) 

				(*
					1 - symbol - ie, goto action
					2 - token - ie,  shift action
					3 - empty - ie , reduce action
				*)
				val map_item_list = map (fn SOME symtok => 
												(
													case AtomSet.member(sym_table,symtok) of
														 true=>( symtok , 1 )
														|false =>( symtok , 2 )
												)
											| NONE => ( Atom.atom " ",3)
										) after_dot_list
				val singleton_map_list = map (AtomMap.singleton) map_item_list
				fun val_union (x,y) = x (*if there is a symbol with two actions then one is taken arbitarily*)
				val final_map = foldl (AtomMap.unionWith val_union) AtomMap.empty singleton_map_list
			in
				(state_num,lr0_set,final_map)
			end


		fun state_closure (state_num,lr0_set,action_map) = 
			let
				val lr0_list = LR0_itemSet.listItems( lr0_set )

				fun findprodns_attachDot lhs = 
					let
						val prod_list = ret_prod_list (rulemap,lhs)
						val new_prod_list = map (fn k=>  (Atom.atom "."::k)  ) prod_list
						val lr0_item_list = map (fn k=> (lhs,k) ) new_prod_list
					in
						LR0_itemSet.fromList(lr0_item_list)
					end

				val new_lr0item_set_list = map (fn (lhs,p)=> (findprodns_attachDot lhs) ) lr0_list
				val new_lr0item_set = foldl (LR0_itemSet.union) lr0_set new_lr0item_set_list

				val next_lr0item_set_list = map (fn (lhs,p)=>  
													case (after_dot p) of
														SOME x=>(
																case AtomSet.member(sym_table,x) of
																	true=>(findprodns_attachDot x)
																	|false=>(LR0_itemSet.empty)
															)
														|NONE =>(LR0_itemSet.empty)
					) (LR0_itemSet.listItems(new_lr0item_set))

				val next_lr0item_set = foldl (LR0_itemSet.union) new_lr0item_set next_lr0item_set_list

			in
				(state_num,next_lr0item_set,action_map)
			end

		fun rec_state_closure (state_num,lr0_set,action_map)=
			let
				val next_state = state_closure (state_num,lr0_set,action_map)

			in
				case LR0_itemSet.equal(lr0_set,#2(next_state)) of
					true => next_state
					|false => rec_state_closure next_state
			end

		fun printState state = 
			let
				val lr0_list = LR0_itemSet.listItems(#2(state))
				val _ = print ("state"^ (Int.toString (#1(state)) ) ^ ":\n" )
				val _ = map ( fn(lhs,prod)=>( print( (Atom.toString lhs)^" ->" );print_production prod;print "\n"  ) ) lr0_list 
				val _ = print ("actios of state"^(Int.toString (#1(state)) )^":\n")
				val _ = AtomMap.appi (fn (k,v) =>
										(
											print ((Atom.toString k)^" has action ");
											(case v of
												1 => print "goto\n"
												|2 => print "shift\n"
												|3 => print "reduce\n"
												|_ =>print "\n"
												) 
										)
								) (#3(state))
			in
				()
			end
	in
		print "lr0:\n";(case AtomMap.find(rulemap,starting_symbol) of
			SOME x=>(print ("given starting symbol is "^(Atom.toString starting_symbol)^"\n" ) ;
				let
					val first_prod = (Atom.atom ".")::(hd   (ret_prod_list(rulemap,starting_symbol))    )
					val state0 = (  0,LR0_itemSet.singleton( (starting_symbol,first_prod)), AtomMap.empty  )
					val state0_closure = create_action_map(rec_state_closure state0)
				in
					printState state0_closure
				end
				)
			|NONE =>(print "false starting symbol\n")
		)
	end
end