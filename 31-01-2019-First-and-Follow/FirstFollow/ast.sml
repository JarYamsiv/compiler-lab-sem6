(* The abstract syntax tree for expression *)
structure Ast = struct

datatype Id = St of Atom.atom | EPSILON | EOP

datatype Rhs = Rh of Id list
 
datatype Rule = Rul of string*Rhs


end
