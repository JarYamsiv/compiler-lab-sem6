(* The abstract syntax tree for expression *)
structure Ast = struct

datatype Id = St of string | EPSILON

datatype Lhs = Lh of Id list
 
datatype Rule = Rul of string*Lhs


end
