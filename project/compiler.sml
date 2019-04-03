val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"

val compileStatus = ref true

structure Key:TAB_KEY_SIG = 
struct
  type ord_key = Atom.atom
  type tab_content = unit
  val compare = Atom.compare 
end

structure SymKey:TAB_KEY_SIG = 
struct
  type ord_key = Atom.atom
  type tab_content = Atom.atom
  val compare = Atom.compare 
end



structure GlobalSymTable = MakeTable(SymKey)

(*function related*)
structure LocalSymTable = MakeTable(SymKey)
val final_ret = ref CAst.VOID

structure GlobalFunctionTable = MakeTable(Key) 


type t_compiled_statement = (int*Ast.Statement)

structure Compiler = 
struct

  fun  compileExpr (Ast.Const x) = (CAst.Const x)
              |compileExpr (Ast.EVar identifier) = 
                let 
                  val _ = case LocalSymTable.getkey(Atom.atom identifier) of
                          SOME tp => (if Atom.compare(tp,Atom.atom "int")=EQUAL then () else 
                                (compileStatus := false ; print (red ^ "type error "^identifier^" not an int \n" ^ reset ))
                            )
                          |NONE => (compileStatus := false ; print (red ^ "undefined identifier "^identifier^"\n" ^ reset ) )
                in 
                  (CAst.EVar identifier)
                end

              |compileExpr (Ast.ARVar (ident,expr)) = (CAst.ARVar (ident,(compileExpr expr)))

              |compileExpr (Ast.Op (e1,oper,e2)) =
                let
                   val c1 = compileExpr e1
                   val c2 = compileExpr e2
                 in
                    case (e1,e2) of
                      (Ast.Const x,Ast.Const y) =>  CAst.Const (CAst.processExpr(x,oper_conv oper,y))
                      |(_,_)                    =>  CAst.Op(c1,oper_conv oper,c2) 
                  
                 end 

          fun compileCondition (Ast.BConst x)   = (CAst.BConst (bool_conv x))

              |compileCondition (Ast.BVar identifier) = 
                let 
                  val _ = case LocalSymTable.getkey(Atom.atom identifier) of
                          SOME tp => (if Atom.compare(tp,Atom.atom "bool")=EQUAL then () else 
                                (compileStatus := false ; print (red ^ "type error "^identifier^" not an bool \n" ^ reset ))
                            )
                          |NONE => (compileStatus := false ; print (red ^ "undefined identifier "^identifier^"\n" ^ reset ) )
                in 
                  (CAst.BVar identifier)
                end


              | compileCondition (Ast.CondOp (x,oper,y)) = 
                let
                  val c1 = compileCondition x
                  val c2 = compileCondition y
                in
                  case oper of
                    Ast.OR => (
                              case (c1,c2) of 
                                (CAst.BConst CAst.TRUE,_) => (CAst.BConst CAst.TRUE) 
                                |(_,CAst.BConst CAst.TRUE) => (CAst.BConst CAst.TRUE)
                                |(_,_) => (CAst.CondOp (c1,condOp_conv oper,c2))
                              )
                    |Ast.AND => (
                                  case (c1,c2) of 
                                  (CAst.BConst CAst.FALSE,_) => (CAst.BConst CAst.FALSE) 
                                  |(_,CAst.BConst CAst.FALSE) => (CAst.BConst CAst.FALSE) 
                                  |(_,_) => (CAst.CondOp (c1,condOp_conv oper,c2))
                                )
                end

               | compileCondition (Ast.Rel(x,oper,y)) = (CAst.Rel(compileExpr x,relOp_conv oper,compileExpr y))
                  
                
          

          (*
            2 - print this statement and those who comes after it
            1 - print this statement and stop
            0 - print none
          *)
          
          fun compileStatement (Ast.As (varname,expr)) =
                let
                  val isdef = LocalSymTable.checkkey(Atom.atom varname)
                  val _ = LocalSymTable.addkey(Atom.atom varname,(Atom.atom "int"))
                  val tp = CAst.INT    
                in
                  (2,(CAst.As (varname,compileExpr expr,tp,isdef)))
                end
            | compileStatement (Ast.BAs (varname,c)) = 
                let
                  val isdef = LocalSymTable.checkkey(Atom.atom varname)
                  val _ = LocalSymTable.addkey(Atom.atom varname,(Atom.atom "bool"))
                  val tp = CAst.BOOL    
                in
                  (2,CAst.BAs(varname,compileCondition c,isdef))
                end

            | compileStatement (Ast.If(c,stls)) = 
                let
                  val compiled_codition = compileCondition c
                  val compiled_statement = compileStatements stls
                in
                    case compiled_codition of
                      CAst.BConst x => 
                      if x=CAst.FALSE then 
                        let 
                          val _ = print (grey^"found dead if statement removing\n"^reset)
                        in
                          (0,CAst.EmptyStatement) 
                        end
                      else
                      (
                        let
                          val _ = print (grey^"found always true if statement removing\n"^reset)
                        in
                          (2,CAst.StList compiled_statement)
                        end
                      )
                      |_ =>
                      (
                        case compiled_statement of
                        [] => (0,CAst.EmptyStatement)
                        |_ => (2,CAst.If(compiled_codition,compiled_statement))
                      )
                end
            | compileStatement (Ast.IfEl(c,stl1,stl2)) = 
                let
                  val compiled_codition = compileCondition c
                  val if_statements = compileStatements stl1
                  val else_statements = compileStatements stl2
                in
                  case compiled_codition of
                      CAst.BConst x => if x=CAst.FALSE then 
                        let 
                          val _ = print (grey^"found dead if in if-else statement removing\n"^reset)
                        in
                          (2,CAst.StList else_statements)
                        end 
                      else
                      (
                        let 
                          val _ = print (grey^"found dead else statement removing\n"^reset)
                        in
                          (2,CAst.StList if_statements)
                        end
                      )
                      |_ =>
                      (
                        (2,CAst.IfEl(compiled_codition,if_statements,else_statements))
                      )
                end
            | compileStatement (Ast.Ret expr) = (final_ret:=CAst.INT;(1,CAst.Ret (compileExpr expr)))
            | compileStatement (Ast.While (c,sl)) = (2,(CAst.While(compileCondition c,compileStatements sl)))
            | compileStatement (Ast.DirectC code) = (2,CAst.DirectC code)
            
            (*| compileStatement x = (2,x)*)
            | compileStatement _ = (2,CAst.EmptyStatement)


          and compileStatements (st::stls) = 
                let
                  val compiled_statement = compileStatement st
                in
                  case (#1 compiled_statement) of
                    2 => [(#2 compiled_statement)]@compileStatements stls
                    |1 => [(#2 compiled_statement)]
                    |_ => []
                end
            | compileStatements []         = []

  
	fun compileFunction (Ast.Fun(name,s_list)) = 
        let
          val _ = print ("Processing Function "^ green ^ name ^ reset ^ "\n")
          val _ = LocalSymTable.reset ()
          val _ = GlobalFunctionTable.addkey(Atom.atom name,())

          val sl = compileStatements s_list
          val ret_type = !final_ret
          val _ = print "\rfunction done......\n"

        in
          CAst.Fun(name,sl,ret_type)
        end


  fun compile (x::xs) = compileElem x::compile xs
      |compile []     = []

  and 
      compileElem  (Ast.Fn function) =        CAst.Fn (compileFunction function)
      |compileElem (Ast.St statement)       = (CAst.St (#2 (compileStatement statement)))

end
