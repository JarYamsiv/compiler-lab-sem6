val red = "\u001b[31;1m"
val green = "\u001b[32;1m"
val white = "\u001b[37;1m"
val yellow = "\u001b[33;1m"
val grey = "\u001b[30;1m"
val reset = "\u001b[0m"

val compileStatus = ref true

fun reg_error x = (compileStatus := false ; print (red ^ x ^ reset ))

fun acomp(x,y) = if Atom.compare(x,y)=EQUAL then true else false

structure FunKey:TAB_KEY_SIG = 
struct
  type ord_key = Atom.atom
  type tab_content = Atom.atom*(Ast.Argument list)
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
val final_ret = ref (Atom.atom "void")

structure GlobalFunctionTable = MakeTable(FunKey) 


type t_compiled_statement = (int*Ast.Statement)

structure Compiler = 
struct


  fun  compileExpr (tp,(Ast.Const x))  = (Atom.atom "int",(Ast.Const x))
      |compileExpr (tp,(Ast.BVal x)) = (Atom.atom "bool",(Ast.BVal x))
      |compileExpr (tp,(Ast.EVar (identifier))) = 
                let 
                  val this_tp = LocalSymTable.getkey(Atom.atom identifier)
                  val _ = case this_tp of
                          SOME x => ()
                          |NONE => (reg_error ("undefined identifier "^identifier^"\n" ^ reset ) )
                  val tp = case this_tp of SOME x=>x | NONE => (Atom.atom "undef")
                in 
                  (tp,(Ast.EVar identifier))
                end

        |compileExpr (tp,(Ast.ARVar (ident,expr))) =
        let
           val c = #2 (compileExpr (tp,expr))
         in
           (tp,(Ast.ARVar (ident,c)))
         end 

        |compileExpr (tp,Ast.EFncl (fn_name,arg_list)) = (tp,Ast.EFncl (fn_name,arg_list))

        |compileExpr (tp,(Ast.Op (e1,oper,e2))) =
          let
             val cp1 =  (compileExpr (tp,e1))
             val cp2 =  (compileExpr (tp,e2))
             val t1 = #1 cp1
             val t2 = #1 cp2
             val c1 = #2 cp1
             val c2 = #2 cp2
             val _ = if Atom.compare(t1,Atom.atom "int") = EQUAL andalso Atom.compare(t2,Atom.atom "int") = EQUAL 
                      then () 
                      else reg_error "Type error\n"
           in
              case (e1,e2) of
                (Ast.Const x,Ast.Const y) =>  (Atom.atom "int",(Ast.Const (Ast.processExpr(x,oper,y))))
                |(_,_)                    =>  (Atom.atom "int",(Ast.Op(c1,oper,c2))) 
            
           end 

        |compileExpr (tp,(Ast.Erel (e1,oper,e2))) =
          let
             val cp1 =  (compileExpr (tp,e1))
             val cp2 =  (compileExpr (tp,e2))
             val t1 = #1 cp1
             val t2 = #1 cp2
             val c1 = #2 cp1
             val c2 = #2 cp2
             val _ = if Atom.compare(t1,Atom.atom "int") = EQUAL andalso Atom.compare(t2,Atom.atom "int") = EQUAL 
                      then () 
                      else reg_error "Type error\n"
           in
              (Atom.atom "bool",Ast.Erel(c1,oper,c2))
            
           end
        |compileExpr (tp,(Ast.Econd (e1,oper,e2))) =
          let
             val cp1 =  (compileExpr (tp,e1))
             val cp2 =  (compileExpr (tp,e2))
             val t1 = #1 cp1
             val t2 = #1 cp2
             val c1 = #2 cp1
             val c2 = #2 cp2
             val _ = if Atom.compare(t1,Atom.atom "bool") = EQUAL andalso Atom.compare(t2,Atom.atom "bool") = EQUAL 
                      then () 
                      else reg_error "Type error\n"
           in
              (Atom.atom "bool",Ast.Econd(c1,oper,c2))
           end




       (* fun compileCondition (Ast.BConst x)   = (Ast.BConst (x))

            |compileCondition (Ast.BVar identifier) = 
              let 
                val _ = case LocalSymTable.getkey(Atom.atom identifier) of
                        SOME tp => (if Atom.compare(tp,Atom.atom "bool")=EQUAL then () else 
                              (compileStatus := false ; print (red ^ "type error "^identifier^" not an bool \n" ^ reset ))
                          )
                        |NONE => (compileStatus := false ; print (red ^ "undefined identifier "^identifier^"\n" ^ reset ) )
              in 
                (Ast.BVar identifier)
              end


            | compileCondition (Ast.CondOp (x,oper,y)) = 
              let
                val c1 = compileCondition x
                val c2 = compileCondition y
              in
                case oper of
                  Ast.OR => (
                            case (c1,c2) of 
                              (Ast.BConst Ast.TRUE,_) => (Ast.BConst Ast.TRUE) 
                              |(_,Ast.BConst Ast.TRUE) => (Ast.BConst Ast.TRUE)
                              |(_,_) => (Ast.CondOp (c1,oper,c2))
                            )
                  |Ast.AND => (
                                case (c1,c2) of 
                                (Ast.BConst Ast.FALSE,_) => (Ast.BConst Ast.FALSE) 
                                |(_,Ast.BConst Ast.FALSE) => (Ast.BConst Ast.FALSE) 
                                |(_,_) => (Ast.CondOp (c1,oper,c2))
                              )
              end

             | compileCondition (Ast.Rel(x,oper,y)) = 
             let
               val c1 = #2 (compileExpr (Atom.atom "undef",x))
               val c2 = #2 (compileExpr (Atom.atom "undef",y))
             in
               (Ast.Rel(c1 ,oper,c2))
             end*)
                  
                
          

          (*
            2 - print this statement and those who comes after it
            1 - print this statement and stop
            0 - print none
          *)
          
          fun compileStatement (Ast.As (varname,expr,tp,isdef)) =
                let
                  val isdef = LocalSymTable.checkkey(Atom.atom varname)
                  val prev_type = LocalSymTable.getkey(Atom.atom varname)


                   
                  val cp = (compileExpr (Atom.atom "undef",expr))  
                  val cpe = #2 cp
                  val tpe = #1 cp

                  val _ = case prev_type of
                          NONE => ()
                          |SOME x => if acomp(x,tpe) then () else reg_error ("Multiple types for " ^ varname ^ "\n")

                  val _ = LocalSymTable.addkey(Atom.atom varname,tpe)

                in
                  (2,(Ast.As (varname,cpe,tpe,isdef)))
                end

            | compileStatement (Ast.If(c,stls)) = 
                let
                  val compiled_codition = #2 (compileExpr (Atom.atom "undef",c))
                  val _ = LocalSymTable.levelup()
                  val compiled_statement = compileStatements stls
                  val _ = LocalSymTable.leveldown()
                in
                    case compiled_codition of
                      Ast.BVal x => 
                      if x=Ast.FALSE then 
                        let 
                          val _ = print (grey^"found dead if statement removing\n"^reset)
                        in
                          (2,Ast.EmptyStatement) 
                        end
                      else
                      (
                        let
                          val _ = print (grey^"found always true if statement removing\n"^reset)
                        in
                          (2,Ast.StList compiled_statement)
                        end
                      )
                      |_ =>
                      (
                        case compiled_statement of
                        [] => (2,Ast.EmptyStatement)
                        |_ => (2,Ast.If(compiled_codition,compiled_statement))
                      )
                end
            | compileStatement (Ast.IfEl(c,stl1,stl2)) = 
                let
                  val compiled_codition = #2 (compileExpr (Atom.atom "undef",c))
                  val if_statements = compileStatements stl1
                  val _ = LocalSymTable.levelup()
                  val else_statements = compileStatements stl2
                  val _ = LocalSymTable.leveldown()
                in
                  case compiled_codition of
                      Ast.BVal x => if x=Ast.FALSE then 
                        let 
                          val _ = print (grey^"found dead if in if-else statement removing\n"^reset)
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

            | compileStatement (Ast.Ret expr) = (
                if acomp(!final_ret,Atom.atom "undef") orelse acomp(!final_ret,Atom.atom "int") then
                  let
                    
                    val cp = (compileExpr (Atom.atom "undef",expr))
                    val cpe = #2 cp
                    val tp = #1 cp
                    val _ = final_ret:=(tp)
                  in
                    (1,Ast.Ret (cpe))
                  end
                
                else
                  let
                    val _ = compileStatus := false
                    val _ = print (red^"Multiple return types for the same function\n"^reset)
                  in
                    (0,Ast.EmptyStatement)
                  end
              )

            | compileStatement (Ast.While (c,sl)) =
            let
              val compiled_expr = #2 (compileExpr (Atom.atom "undef",c))

              val _ = LocalSymTable.levelup()
              val compiled_statement = compileStatements sl
              val _ = LocalSymTable.leveldown()
               
             in
               (2,(Ast.While(compiled_expr ,compiled_statement)))
             end 
            | compileStatement (Ast.DirectC code) = (2,Ast.DirectC code)
            
            (*| compileStatement x = (2,x)*)
            | compileStatement _ = (2,Ast.EmptyStatement)


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

  
	fun compileFunction (Ast.Fun(name,s_list,ret_type,arg_list)) = 
        let
          val _ = final_ret:=(Atom.atom "undef")
          val _ = print ("Processing Function "^ green ^ name ^ reset ^ "\n")
          val _ = LocalSymTable.reset ()
          

          val sl = compileStatements s_list
          val ret_type = if acomp(!final_ret,Atom.atom "undef") then (Atom.atom "void") else !final_ret

          val _ = GlobalFunctionTable.addkey(Atom.atom name,(ret_type,arg_list))

          val _ = print "\rfunction done......\n"

        in
          Ast.Fun(name,sl,ret_type,arg_list)
        end


  fun compile (x::xs) = compileElem x::compile xs
      |compile []     = []

  and 
      compileElem  (Ast.Fn function) =        Ast.Fn (compileFunction function)
      |compileElem (Ast.St statement)       = (Ast.St (#2 (compileStatement statement)))

end
