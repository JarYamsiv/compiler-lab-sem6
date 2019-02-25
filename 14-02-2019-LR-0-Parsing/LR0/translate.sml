structure Translate =
struct



val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"







fun compileProdElem y :(Atom.atom option) = 
	case y of
				(Ast.St x) => (print((Atom.toString x)^" ");  (SOME x)  )
				|(Ast.EPSILON) => (print("EPSILON");   NONE  )
				|(Ast.EOP) => (print("dollar");  (SOME (Atom.atom "$"))  ) 



fun compileRHS (Ast.Rh y)  :HelpFun.RHS    =HelpFun.filter HelpFun.id (map compileProdElem y)

fun compileRule (Ast.Rul(x,y)) = let 
									val _ = print (x^"->")
									val atom_val_x = (Atom.atom x)
									val prod_list = compileRHS y
									val tok_set = AtomSet.fromList(prod_list)
									val ret_list_set = HelpFun.ProductionSet.singleton(prod_list)

								in
								(	
									print ("\n");
									(atom_val_x,ret_list_set,tok_set)
									
								 )end


fun compile l (grammar:HelpFun.Grammar_t)  :HelpFun.Grammar_t= 
	case l of
				(x::xs) =>	( let
									val rulemap = #rules grammar
									val sym_table = #sym_table grammar
									val tok_table = #tok_table grammar

									val a = compileRule x
									val lhskey_compiled    = #1(a)
									val prodnlist_compiled = #2(a)
									val this_tok_table = #3(a)

									val this_map = AtomMap.singleton(lhskey_compiled,prodnlist_compiled)
									val this_sym_table = AtomSet.singleton(lhskey_compiled)									
									val (new_gram) = compile xs grammar

									val new_map = #rules (new_gram)
									val new_sym_table = #sym_table (new_gram)
									val new_tok_table = #tok_table (new_gram)

									val ret_sym_table = AtomSet.union(this_sym_table,new_sym_table)
									val ret_tok_table = AtomSet.union(new_tok_table,this_tok_table)
									val ret_tok_table = AtomSet.difference(ret_tok_table,ret_sym_table)
									val ret_tok_table = AtomSet.difference(ret_tok_table,AtomSet.singleton(Atom.atom "_"))
									

								in 
									  ( {
									  		rules= AtomMap.unionWith (HelpFun.ProductionSet.union) (this_map,new_map) , 
									  		sym_table= ret_sym_table ,
									  		tok_table= ret_tok_table  
									  	}
									  	) 
						    	end)
				| []	=> ({
								rules= AtomMap.empty,
								sym_table= AtomSet.empty,
								tok_table= AtomSet.empty
					}
					)

(*there is something wrong with the tok_table*)

fun printmap (gram:HelpFun.Grammar_t)=
	let
		val rulemap = #rules gram
		val sym_table = #sym_table gram
		val tok_table = #tok_table gram
		fun print_prodns rhs_from_map=
					let
						val rhs_from_map_e =valOf rhs_from_map
						val prodn_list = map HelpFun.RHSKey.convToRhs (HelpFun.ProductionSet.listItems rhs_from_map_e)



						fun print_symtok x = case AtomSet.member(sym_table,x) of
							true => (print (green^(Atom.toString x)^reset^" "))
							|false => (print (red^(Atom.toString x)^reset^" "))

						fun print_prodn p =(case p of 
							(x::xs) => map print_symtok (x::xs)
							|_		=> (print "_";[()]) )

						fun  prnt_rhs_list [x] = (print_prodn x;())
							|prnt_rhs_list []	=()
							|prnt_rhs_list (x::xs)=(print_prodn x;print " | ";prnt_rhs_list xs)
					in
						(prnt_rhs_list prodn_list)
					end
		fun print_rule symbol = (print (green^(Atom.toString symbol)^reset^"->");print_prodns (AtomMap.find(rulemap,symbol));print "\n")
	in
		map print_rule (AtomSet.listItems sym_table)
	end

fun ret_prod_list (rulemap,lhs):HelpFun.RHS list = map HelpFun.RHSKey.convToRhs (HelpFun.ProductionSet.listItems (valOf  (AtomMap.find (rulemap,lhs))))




(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*======================================================CALCULATION OF NULLABLE=====================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)

fun calc_nullable (gram:HelpFun.Grammar_t) = let

	val rulemap = #rules gram
	val sym_table = #sym_table gram
	val tok_table = #tok_table gram

	fun next_nullable_set cur_set =
		let
			val string_nullable = HelpFun.forall (fn k=>  AtomSet.member(cur_set,k)  )

			fun singularProdn lhs = 
				let
					val prod_list = ret_prod_list (rulemap,lhs)
					val sing_prodn = HelpFun.filter (fn k=>case (string_nullable k) of true =>SOME k |false =>NONE) prod_list
				in
					case sing_prodn of [] =>(NONE) |_ =>(SOME lhs)
				end
			val next_set = HelpFun.filter singularProdn (AtomSet.listItems sym_table)
		in
			AtomSet.fromList(next_set)
		end

	val fixedPointNullable = HelpFun.MakeFixedPointFn (next_nullable_set) (AtomSet.equal)

	val nullable_set = fixedPointNullable AtomSet.empty

	val _ = print "nullable set:\n"
in
	HelpFun.PrintSet nullable_set;
	nullable_set
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



fun calc_first (gram:HelpFun.Grammar_t,nullable_set)=
	let
		val rulemap = #rules gram
		val sym_table = #sym_table gram
		val tok_table = #tok_table gram

		fun base_first lhs=
			let
				val prod_list = ret_prod_list (rulemap,lhs)
			in
				AtomSet.fromList( HelpFun.filter (
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


	(*fun calc_follow(rulemap,sym_table,tok_table,nullable_set,first_map)=
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
		end*)

(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*===================================================CALCULATION OF LR(0) TABLE=====================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)
(*==================================================================================================================================*)

(*
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




*)
(*end of translate structure*)
end



(*
	Type definitions:

	Map : 
		Key : Atom.atom
		Element: HelpFun.ProductionSet.set




*)

