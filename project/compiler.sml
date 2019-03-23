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
          val final_ret = ref Ast.VOID


          fun  compileExpr (Ast.Const x) = (Ast.Const x)
              |compileExpr (Ast.EVar identifier) = 
                let 
                  val _ = case LocalSymTable.checkkey(Atom.atom identifier) of
                          true => ()
                          |false => (compileStatus := false ; print (red ^ "undefined identifier "^identifier^"\n" ^ reset ) )
                in 
                  (Ast.EVar identifier)
                end

              |compileExpr (Ast.ARVar (ident,expr)) = (Ast.ARVar (ident,(compileExpr expr)))

              |compileExpr (Ast.Op (e1,oper,e2)) =
                let
                   val c1 = compileExpr e1
                   val c2 = compileExpr e2
                 in
                    case (e1,e2) of
                      (Ast.Const x,Ast.Const y) =>  Ast.Const (Ast.processExpr(x,oper,y))
                      |(_,_)                    =>  Ast.Op(c1,oper,c2) 
                  
                 end 
          

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
                  (2,(Ast.As (varname,compileExpr expr,tp,isdef)))
                end
            | compileStatement (Ast.If(c,stls)) = 
                let
                  val compiled_statement = compileStatements stls
                in
                    case compiled_statement of
                      [] => (0,Ast.EmptyStatement)
                      |_ => (2,Ast.If(c,compiled_statement))
                end
            | compileStatement (Ast.IfEl(c,stl1,stl2)) = (2,(Ast.IfEl(c,compileStatements stl1,compileStatements stl2)))
            | compileStatement (Ast.Ret expr) = (final_ret:=Ast.INT;(1,Ast.Ret expr))
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
          val ret_type = !final_ret
          val _ = print "\rfunction done......\n"

        in
          Ast.Fun(name,sl,ret_type)
        end
end
