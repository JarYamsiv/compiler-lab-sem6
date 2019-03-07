structure Util = 
struct
	
end

signature TAB_SIG = 
sig
	type key
	type tab_content
	type proxy

	val addkey			: key*tab_content -> unit
	val addKeyReplace	: key*tab_content -> unit
	val checkkey 		: key -> bool
	val getkey			: key -> tab_content option
end

signature TAB_KEY_SIG = 
sig
	type ord_key
	type tab_content
	val compare : ord_key*ord_key -> order 
end

functor MakeSymTable (A:TAB_KEY_SIG):TAB_SIG = 
struct
	type key = A.ord_key
	type tab_content = A.tab_content
	type proxy = int

	structure Map = RedBlackMapFn(A)

	val m = ref Map.empty:tab_content Map.map ref

	fun addkey (x,t) = case Map.find(!m,x) of NONE => (m := Map.insert(!m,x,t)) | SOME x => () 

	fun addKeyReplace (x,t) = (m := Map.insert(!m,x,t))
	
	fun checkkey x = case Map.find(!m,x) of SOME x=> true | NONE => false
	
	fun getkey x = Map.find(!m,x)
end