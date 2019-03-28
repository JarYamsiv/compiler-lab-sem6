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
          val _ = GlobalFunctionTable.addkey(Atom.atom name,())


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

          fun compileCondition (Ast.BConst x)   = (Ast.BConst x)


              | compileCondition (Ast.CondOp (x,oper,y)) = 
                let
                  val c1 = compileCondition x
                  val c2 = compileCondition y
                in
                  case oper of
                    Ast.OR => (
                              case (c1,c2) of 
                                (Ast.BConst x,_) => if x=Ast.TRUE then (Ast.BConst Ast.TRUE) else (Ast.CondOp (c1,oper,c2))
                                |(_,Ast.BConst x) => if x=Ast.TRUE then (Ast.BConst Ast.TRUE) else (Ast.CondOp (c1,oper,c2))
                                |(_,_) => (Ast.CondOp (c1,oper,c2))
                              )
                    |Ast.AND => (
                                  case (c1,c2) of 
                                  (Ast.BConst x,_) => if x=Ast.FALSE then (Ast.BConst Ast.FALSE) else (Ast.CondOp (c1,oper,c2))
                                  |(_,Ast.BConst x) => if x=Ast.FALSE then (Ast.BConst Ast.FALSE) else (Ast.CondOp (c1,oper,c2))
                                  |(_,_) => (Ast.CondOp (c1,oper,c2))
                                )
                  
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
                  val compiled_codition = compileCondition c
                  val compiled_statement = compileStatements stls
                in
                    case compiled_codition of
                      Ast.BConst x => if x=Ast.FALSE then 
                        let 
                          val _ = print (grey^"found dead if statement removing\n"^reset)
                        in
                          (0,Ast.EmptyStatement) 
                        end
                      else
                      (
                        (2,Ast.StList compiled_statement)
                      )
                      |_ =>
                      (
                        case compiled_statement of
                        [] => (0,Ast.EmptyStatement)
                        |_ => (2,Ast.If(compiled_codition,compiled_statement))
                      )
                end
            | compileStatement (Ast.IfEl(c,stl1,stl2)) = 
                let
                  val compiled_codition = compileCondition c
                  val if_statements = compileStatements stl1
                  val else_statements = compileStatements stl2
                in
                  case compiled_codition of
                      Ast.BConst x => if x=Ast.FALSE then 
                        let 
                          val _ = print (grey^"found dead if statement removing\n"^reset)
                        in
                          (2,Ast.StList else_statements)
                        end 
                      else
                      (
                        let 
                          val _ = print (grey^"found dead else statement removing\n"^reset)
                        in
                          (2,Ast.StList if_statements)
                        end
                      )
                      |_ =>
                      (
                        (2,Ast.IfEl(compiled_codition,if_statements,else_statements))
                      )
                end
            | compileStatement (Ast.Ret expr) = (final_ret:=Ast.INT;(1,Ast.Ret expr))
            | compileStatement (Ast.While (c,sl)) = (2,(Ast.While(compileCondition c,compileStatements sl)))
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
