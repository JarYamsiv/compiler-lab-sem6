val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"

structure SymKey:TAB_KEY_SIG = 
struct
  type ord_key = Atom.atom
  type tab_content = unit
  val compare = Atom.compare 
end

structure GlobalSymTable = MakeTable(SymKey)
structure LocalSymTable = MakeTable(SymKey)

structure Compiler = 
struct
	fun compileFunction (Ast.Fun(name,s_list,tp)) = 
        let
          val _ = print ("Processing Function "^ green ^ name ^ reset ^ "\n")
          val _ = LocalSymTable.reset ()
          val ret_type = tp
          val sl = s_list
        in
          Ast.Fun(name,sl,ret_type)
        end
end
