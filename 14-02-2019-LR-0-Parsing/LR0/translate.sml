structure Translate =
struct


val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"

val isStart = ref 0
val startSym = ref (Atom.atom "S")

fun change x =  (isStart := 1; startSym := (Atom.atom x));


fun compileProdElem y :(Atom.atom option) = 
	case y of
				(Ast.St x) => (print((Atom.toString x)^" ");  (SOME x)  )
				|(Ast.EPSILON) => (print("_");   NONE  )
				|(Ast.EOP) => (print("$");  (SOME (Atom.atom "$"))  ) 



fun compileRHS (Ast.Rh y)  :HelpFun.RHS    =HelpFun.filter HelpFun.id (map compileProdElem y)

fun compileRule (Ast.Rul(x,y)) = let
									val _ =if !isStart = 0 then (change x) else ()

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
									  		tok_table= ret_tok_table  ,
									  		starting_sym = !startSym
									  	}
									  	) 
						    	end)
				| []	=> ({
								rules= AtomMap.empty,
								sym_table= AtomSet.empty,
								tok_table= AtomSet.empty,
								starting_sym = !startSym
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

	(*up - for each production (lsh->prod) it will return a set if lhs is nulalble from that prodn empty o/w*)
	fun up cur_set (lhs,prod) = if (HelpFun.forall (fn k=>  AtomSet.member(cur_set,k)  )) prod then AtomSet.singleton(lhs) else AtomSet.empty

	fun upd cur_set = foldl (AtomSet.union) (AtomSet.empty) (HelpFun.map_lhs_productions gram  (up cur_set)   )

	val fixedPointNullable = HelpFun.MakeFixedPointFn (upd) (AtomSet.equal)

	val nullable_set = fixedPointNullable AtomSet.empty

	val _ = print "nullable set:"
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
		val sym_list = AtomSet.listItems sym_table

		fun lhs_update lhs cur_map=
			let
				val prod_list = ret_prod_list (rulemap,lhs)

				fun first_of_current_prodn (symtok::symtok_ls) = 
					(
						if AtomSet.member(sym_table,symtok) then
							let
								val this_symtok_first = (case AtomMap.find(cur_map,symtok) of SOME x=>(x) |NONE =>(AtomSet.empty) )
							in
								if AtomSet.member(nullable_set,symtok) then
									AtomSet.union(this_symtok_first,first_of_current_prodn symtok_ls)
								else
									this_symtok_first
							end
						else
							AtomSet.singleton(symtok)
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

		val mapCompare = HelpFun.MakeMapCompareFn (AtomMap.listKeys) (fn (x,y)=>Atom.compare(x,y)=EQUAL) (AtomMap.find) (AtomSet.equal)


		val FPupdateFn = fn k=>( AtomMap.unionWith AtomSet.union  (k, calc_next_map sym_list k)  )
		val FPcompareFn = mapCompare
		val FP_Fn = HelpFun.MakeFixedPointFn (FPupdateFn) (FPcompareFn)

		val final_map = FP_Fn AtomMap.empty

	in
		
		print "first elements map:\n";
		map (fn k=>(print ((Atom.toString k)^"->");
						(
							case AtomMap.find(final_map,k) of
								SOME x=>HelpFun.PrintSet x
								|NONE =>()
						)
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


fun calc_follow(gram:HelpFun.Grammar_t,nullable_set,first_map)=
	let
		val rulemap = #rules gram
		val sym_table = #sym_table gram
		val tok_table = #tok_table gram
		val lhs_list = AtomSet.listItems(sym_table)

		fun string_first (symtok::symtok_ls) f_map= 
				(
				case AtomSet.member(sym_table,symtok) of
					false   =>(AtomSet.singleton(symtok))
					|true =>(
						let 
							val this_symtok_first = (case AtomMap.find(f_map,symtok) of SOME x=>(x) |NONE =>(AtomSet.empty))
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


val sts = ref HelpFun.IntMap.empty:HelpFun.LR0_itemSet.set HelpFun.IntMap.map ref
val its = ref HelpFun.ItemMap.empty:int HelpFun.ItemMap.map ref
val ltoi = ref HelpFun.SetMap.empty:int HelpFun.SetMap.map ref

val state_count = ref 0
val States = ref []: (HelpFun.state_t list) ref

fun is_lr0_present (lr0)  =
	let
		val current_lr0s = HelpFun.ItemMap.listKeys(!its)
	in
		HelpFun.there_exists (fn k=> (HelpFun.compare_lr0 (k,lr0)) = EQUAL ) current_lr0s
	end

fun lr0_intersect (lr0_set) = 
	let
		val lr0_list = HelpFun.LR0_itemSet.listItems(lr0_set)
		val x =map is_lr0_present lr0_list
	in
		HelpFun.forall (HelpFun.id) x
	end

(*fun is_set_present (lr0_set)  = HelpFun.IntMap.exists (fn k=>HelpFun.LR0_itemSet.equal(lr0_set,k)) (!sts)*)


fun print_production prodn = (
									print "[";
									map (fn k=>print (  (Atom.toString k)^" "  )) prodn;
									print "]"
								)

fun printlr0 (lhs,prod) = (print( "\t"^(Atom.toString lhs)^" ->" );print_production prod;print "\n")
fun prntlr0set set = map (printlr0) (HelpFun.LR0_itemSet.listItems set) 


fun calc_lr0 (
	gram:HelpFun.Grammar_t,
	nullable_set,
	first_map,
	follow_map
	)=
	let
		val rulemap = #rules gram
		val sym_table = #sym_table gram
		val tok_table = #tok_table gram
		val starting_symbol = #starting_sym gram	



		fun after_dot (x::xs) = (case Atom.compare(x,Atom.atom ".") of
									EQUAL => (case xs of
												(y::ys) => SOME y
												|[]		=> NONE
											 )
									|_ =>(after_dot xs))
			| after_dot []	  = NONE

		fun before_dot p = after_dot (List.rev p)

		fun move_dot (y::ys) (x::xs) = (case Atom.compare(x,Atom.atom ".")  of 
											EQUAL =>( case xs of 
														[] => (y::ys)@[x]
														|(z::zs) => (y::ys)@[z]@[x]@zs )
											|_ => move_dot ((y::ys)@[x]) xs

										)
			|move_dot [] 	(x::xs)	 =  (case Atom.compare(x,Atom.atom ".")  of 
											EQUAL =>( case xs of 
														[] => [x]
														|(z::zs) => [z]@[x]@zs )
											|_ => move_dot [x] xs

										)
			|move_dot temp  []		 = temp


		fun filter_dot (x:Atom.atom) lr0_set = 
			let
				val lr0_list = HelpFun.LR0_itemSet.listItems(lr0_set)
				val filter_x = map (fn (lhs,rhs)=>

					(case after_dot(rhs) of 
						NONE => NONE
						| SOME y=> (if Atom.compare(y,x)=EQUAL then (SOME (lhs,move_dot [] rhs)) else NONE)

						) )lr0_list
				val new_lr0_list = HelpFun.filter (HelpFun.id) filter_x
				val _ = map ( fn(lhs,prod)=>( print( "\t"^(Atom.toString lhs)^" ->" );print_production prod;print "\n"  ) ) new_lr0_list 
			in
				HelpFun.LR0_itemSet.fromList(new_lr0_list)
			end

		fun create_action_map (state_num,lr0_set,action_map) = 
			let
				val lr0_list = HelpFun.LR0_itemSet.listItems(lr0_set)
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


		fun state_closure (lr0_set) = 
			let
				val lr0_list = HelpFun.LR0_itemSet.listItems( lr0_set )

				fun findprodns_attachDot lhs = 
					let
						val prod_list = ret_prod_list (rulemap,lhs)
						val new_prod_list = map (fn k=>  (Atom.atom "."::k)  ) prod_list
						val lr0_item_list = map (fn k=> (lhs,k) ) new_prod_list
					in
						HelpFun.LR0_itemSet.fromList(lr0_item_list)
					end

				val updatable_lhs:Atom.atom list = HelpFun.filter (HelpFun.id) (map (fn(lhs,rhs)=>(after_dot rhs)) lr0_list)
				val updatable_lhs = List.filter (fn k=>AtomSet.member(sym_table,k)) updatable_lhs

				val new_lr0item_set_list = map (fn (lhs,p)=> (findprodns_attachDot lhs) ) lr0_list
				val new_lr0item_set_list = map (findprodns_attachDot) updatable_lhs

				val new_lr0item_set = foldl (HelpFun.LR0_itemSet.union) lr0_set new_lr0item_set_list

				val next_lr0item_set = HelpFun.LR0_itemSet.union(lr0_set,new_lr0item_set)

			in
				(next_lr0item_set)
			end

		fun rec_state_closure (state_num,lr0_set,action_map)=
			let
				val next_set = state_closure (lr0_set)

			in
				case HelpFun.LR0_itemSet.equal(lr0_set,next_set) of
					true => (state_num,next_set,action_map)
					|false => rec_state_closure (state_num,next_set,action_map)
			end

		fun rec_set_closure (lr0_set)=
			let
				val next_set = state_closure (lr0_set)
			in
				case HelpFun.LR0_itemSet.equal(lr0_set,next_set) of
					true => (next_set)
					|false => rec_set_closure (next_set) 
			end

		fun printState state = 
			let
				val lr0_list = HelpFun.LR0_itemSet.listItems(#2(state))
				val _ = print ("state"^ (Int.toString (#1(state)) ) ^ ":\n" )
				val _ = map ( fn(lhs,prod)=>( print( (Atom.toString lhs)^" ->" );print_production prod;print "\n"  ) ) lr0_list 
				val _ = print ("actios of state"^(Int.toString (#1(state)) )^":\n")
				val _ = AtomMap.appi (fn (k,v) =>
										(
											let
												val new_lr0_set = (filter_dot k (#2 state))
												val lst = HelpFun.LR0_itemSet.listItems(new_lr0_set)
												val id =(case lst of  
															(x::xs) => (case HelpFun.ItemMap.find(!its,(hd lst)) of
																		SOME x =>x
																		|NONE => ~1)
															|_		=> (~1)
															)
											in
											print ("on "^(Atom.toString k)^" has action ");
											(case v of
												1 => (print ("goto to "^(Int.toString id)^"\n") )
												|2 => (print ("shift to "^(Int.toString id)^"\n")  )
												|3 => print "reduce\n"
												|_ =>print "\n"
												) 
											end
										)
								) (#3(state))
			in
				()
			end

		fun process_state (state:HelpFun.state_t) = 
			let
				val num:int = #1(state)
				val lr0_set:HelpFun.LR0_itemSet.set = #2(state)
				val action_map:int AtomMap.map= #3(state)

				val lr0_list = HelpFun.LR0_itemSet.listItems(lr0_set)

				fun add_lr0_to_state_map (x::xs) = ( its:=HelpFun.ItemMap.insert(!its,x,num) ; add_lr0_to_state_map xs )
					|add_lr0_to_state_map []	 = ()

				val _ = ltoi := HelpFun.SetMap.insert(!ltoi,lr0_set,num)
				val _ = add_lr0_to_state_map lr0_list
				val _ = sts := HelpFun.IntMap.insert(!sts,num,lr0_set)

			in
				()
			end

		fun propogate_from_state (state:HelpFun.state_t) = 
			let
				val action_map = #3 (state)
				val lr0_set = #2 (state)
				val action_symbols = AtomMap.listKeys (action_map)

				fun for_action (a:Atom.atom) = 
					let
						val new_lr0_set= rec_set_closure(filter_dot a lr0_set)
						val isPresent = lr0_intersect new_lr0_set
						val isPresent = case HelpFun.SetMap.find(!ltoi,new_lr0_set) of SOME x=>true|NONE =>false
					in
						case isPresent of
							true => false
							| false => (
								let
									val _ = (state_count := (!state_count)+1)
									val new_state_temp = (!state_count,new_lr0_set,AtomMap.empty)
									val new_state = create_action_map(rec_state_closure new_state_temp)
									val _ = process_state(new_state)
									val _ = (States := (!States)@[new_state])
								in
									true
								end

								)
					end
			in
				HelpFun.there_exists (HelpFun.id) (map for_action action_symbols)
			end

			fun print_states (x::xs) = (print "\n"; printState x; print_states xs;())
				|print_states []	 = ()
	in
		print (red^"\nlr0:\n"^reset) ;(case AtomMap.find(rulemap,starting_symbol) of
			SOME x=>(print ("given starting symbol is "^(Atom.toString starting_symbol)^"\n\n\n" ) ;
				let
					val first_prod = (Atom.atom ".")::(hd   (ret_prod_list(rulemap,starting_symbol))    )
					val state0 = (  (!state_count),HelpFun.LR0_itemSet.singleton( (starting_symbol,first_prod)), AtomMap.empty  )
					val state0_closure = create_action_map(rec_state_closure state0)
					val _ = process_state state0_closure
					val _ = States := !States@[state0_closure]
					fun rec_prop x = case propogate_from_state(List.last(!States)) of
										false => ()
										|true => rec_prop x
					val _ = rec_prop 10
				in
					print_states (!States)
				end
				)
			|NONE =>(print "false starting symbol\n")
		)
	end


(*end of translate structure*)
end



(*
	Type definitions:

	Map : 
		Key : Atom.atom
		Element: HelpFun.ProductionSet.set




*)

