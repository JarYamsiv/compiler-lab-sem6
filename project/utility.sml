structure Util = 
struct
	
end

exception level_underflow

signature TAB_SIG = 
sig
	type key
	type tab_content
	type proxy

	val addkey			: key*tab_content -> unit
	val editkey			: key*tab_content -> unit
	val addKeyReplace	        : key*tab_content -> unit
	val checkkey 		        : key -> bool
	val getkey			: key -> tab_content option
    val reset                       : unit -> unit
    val levelup 				    : unit -> unit
    val leveldown					: unit -> unit
    val flevelup 					: unit -> unit
    val fleveldown 					: unit -> unit
end

signature TAB_KEY_SIG = 
sig
	type ord_key
	type tab_content
	val compare : ord_key*ord_key -> order 
end

functor MakeTable (A:TAB_KEY_SIG):TAB_SIG = 
struct
	type key = A.ord_key
	type tab_content = A.tab_content
	type proxy = int
	type level_t = int
	type funct_level = int

	structure new_key = 
	struct
		type ord_key = A.ord_key*int
		fun compare(x:ord_key,y:ord_key) = case A.compare(#1 x,#1 y) of
			EQUAL => Int.compare(#2 x,#2 y)
			|x => x
	end


	structure Map = RedBlackMapFn(new_key)

	val level = ref 0:level_t ref
	val f_level = ref 0:funct_level ref

	val m = ref Map.empty:(tab_content*level_t*funct_level) Map.map ref

	fun newFind (x) = case Map.find(!m,(x,!f_level)) of SOME (x,l,fl) => if fl = !f_level then SOME(x,l,fl) else NONE | NONE => NONE
	fun newInsert (x,t,l,fl) = m:= Map.insert(!m,(x,!f_level),(t,l,fl))

	fun addkey (x,t) = case (newFind x) of NONE => (newInsert(x,t,!level,!f_level)) | SOME x => () 

	fun editkey (x,t) = case (newFind x) of NONE => (newInsert(x,t,!level,!f_level))
						| SOME (content,l,fl) => (newInsert(x,t,l,fl))

	fun addKeyReplace (x,t) = (newInsert(x,t,!level,!f_level))
	
	fun checkkey x = case (newFind x) of SOME (x,l,fl)=> true  | NONE => false
	
	fun getkey x = case (newFind x) of SOME (t,l,fl) =>  SOME t  | NONE => NONE

    fun reset () = m:=Map.empty

    fun levelup () = (level:= !level +1)

    fun leveldown () = 
    	let
    		val _ = if !level =0 then raise level_underflow else ()
    		val _ = level := !level -1
    		fun fltr (key,(content,l,fl)) = if fl <> !f_level then true else
    			if l > !level then false else true
    		val _ = m := Map.filteri fltr (!m)

    	in
    		()
    	end

    fun flevelup () = (f_level := !f_level +1)
    fun fleveldown () = 
    	let
    		val _ = (f_level:= !f_level -1)
    		fun fltr (key,(content,l,fl)) = if fl > !f_level then false else true
    		val _ = m := Map.filteri fltr (!m)		
    	in
    		()
    	end
end
