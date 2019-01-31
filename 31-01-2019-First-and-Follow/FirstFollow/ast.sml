(* The abstract syntax tree for expression *)
structure Ast = struct

datatype Lhs = Lh of string list
 
datatype Rule = Rul of string*Lhs


end
