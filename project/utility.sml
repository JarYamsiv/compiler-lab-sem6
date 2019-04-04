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
	val addKeyReplace	        : key*tab_content -> unit
	val checkkey 		        : key -> bool
	val getkey			: key -> tab_content option
    val reset                       : unit -> unit
    val levelup 				    : unit -> unit
    val leveldown					: unit -> unit
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

	structure Map = RedBlackMapFn(A)

	val level = ref 0:level_t ref

	val m = ref Map.empty:(tab_content*level_t) Map.map ref

	fun addkey (x,t) = case Map.find(!m,x) of NONE => (m := Map.insert(!m,x,(t,!level))) | SOME x => () 

	fun addKeyReplace (x,t) = (m := Map.insert(!m,x,(t,!level)))
	
	fun checkkey x = case Map.find(!m,x) of SOME x=> true | NONE => false
	
	fun getkey x = case Map.find(!m,x) of SOME (t,l) => SOME t | NONE => NONE

    fun reset () = m:=Map.empty

    fun levelup () = (level:= !level +1)

    fun leveldown () = 
    	let
    		val _ = if !level =0 then raise level_underflow else ()
    		val _ = level := !level -1
    		fun fltr (key,(content,l)) = if l > !level then false else true
    		val _ = m := Map.filteri fltr (!m)

    	in
    		()
    	end
end
