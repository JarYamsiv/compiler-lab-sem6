structure Compiler = 
struct
	fun compileFunction (function) = function
	fun FRetType (stmnt_list) = List.filter (fn k=> case k of (Ast.Ret e) =>true|_ => false )  stmnt_list
end