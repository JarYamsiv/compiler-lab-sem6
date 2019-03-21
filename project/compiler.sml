val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"

val compileStatus = ref true

structure SymKey:TAB_KEY_SIG = 
struct
  type ord_key = Atom.atom
  type tab_content = unit
  val compare = Atom.compare 
end

structure GlobalSymTable = MakeTable(SymKey)
structure LocalSymTable = MakeTable(SymKey)

structure GlobalFunctionTable = MakeTable(SymKey) 


type t_compiled_statement = (int*Ast.Statement)

structure Compiler = 
struct

  

	fun compileFunction (Ast.Fun(name,s_list,tp)) = 
        let
          val _ = print ("Processing Function "^ green ^ name ^ reset ^ "\n")
          val _ = LocalSymTable.reset ()
          

          (*
            2 - print this statement and those who comes after it
            1 - print this statement and stop
            0 - print none
          *)
          
          fun compileStatement (Ast.As (varname,expr,tp,isdef)):t_compiled_statement =
                let
                  val isdef = LocalSymTable.checkkey(Atom.atom varname)
                  val _ = LocalSymTable.addkey(Atom.atom varname,())
                  val tp = Ast.INT    
                in
                  (2,(Ast.As (varname,expr,tp,isdef)))
                end
            | compileStatement (Ast.Ret expr) = (1,Ast.Ret expr)
            | compileStatement x = (2,x)


          and compileStatements (st::stls) = 
                let
                  val compiled_statement:t_compiled_statement = compileStatement st
                in
                  case (#1 compiled_statement) of
                    2 => [(#2 compiled_statement)]@compileStatements stls
                    |1 => [(#2 compiled_statement)]
                    |_ => []
                end
            | compileStatements []         = []

          val sl = compileStatements s_list
          val ret_type = tp
          val _ = print "\rfunction done......\n"

        in
          Ast.Fun(name,sl,ret_type)
        end
end
