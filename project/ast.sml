(* The abstract syntax tree for expression *)
structure Ast = struct

datatype Program_t = 
	C_Program of ProgramElements_t list

and
	ProgramElement_t = 
	C_Statement of int*Statement_t

and
	Statement_t = 
	C_St_Identifier of int*string
	C_St_FunCall of int*string



end
