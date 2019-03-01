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


		val start_symbol = #starting_sym gram
		val first_prodn =hd (ret_prod_list (rulemap,start_symbol))

		val first_lr0_item = {lhs=start_symbol , bef=[] , aft=first_prodn}

		val st0_base_set = HelpFun.ItemSet.singleton(first_lr0_item)

		fun ret_base_lr0_of_sym sym=
			let
				val prod_list = ret_prod_list(rulemap,sym)
				val item_list =map (fn k=>{lhs=sym,bef=[],aft=k}) prod_list
			in
				HelpFun.ItemSet.fromList(item_list)
			end

		fun set_closure (set) = 
			let
				val lr0_list = HelpFun.ItemSet.listItems(set)
				val afterdot_symtok_list =HelpFun.filter  (fn k=>(case k of (x::xs)=> SOME x | [] => NONE)) (map #aft lr0_list) 
				val issym = fn k=>(case AtomSet.member(sym_table,k) of true=>SOME k| false => NONE)
				val potential_sym_list =HelpFun.filter HelpFun.id (map issym afterdot_symtok_list)
			in
				foldl (HelpFun.ItemSet.union) (set) (map ret_base_lr0_of_sym potential_sym_list)
			end

		val rec_set_closure = HelpFun.MakeFixedPointFn (set_closure) (fn (x,y) => HelpFun.ItemSet.compare(x,y) = EQUAL)

	val _ = print (red^"\n=====LR0=====\n\n"^reset)
	in
		HelpFun.printItemSet  (rec_set_closure st0_base_set)
	end


(*end of translate structure*)
end



(*
	Type definitions:

	Map : 
		Key : Atom.atom
		Element: HelpFun.ProductionSet.set




*)

