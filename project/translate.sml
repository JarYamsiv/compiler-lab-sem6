structure Translate =
struct



fun addtabs n = if n <= 0 then
     ("")
    else
     ("   "^( addtabs (n-1)) )


(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(**************************************************translate  EXPRESSION*****************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)





fun translateArgument_f (Ast.Arg (arg_name,arg_type)) = (Atom.toString arg_type)^" "^arg_name

fun translateArguments_f [x]     = translateArgument_f x
    |translateArguments_f (x::xs) = (translateArgument_f x ^" , "^ translateArguments_f xs)
    
    |translateArguments_f []      = ""


fun translateExpr (Ast.Const (x) )         = (" "^(Int.toString x)^" ")


  | translateExpr (Ast.BVal Ast.TRUE)      = ("true ")
  | translateExpr (Ast.BVal Ast.FALSE)      = ("false ")

  | translateExpr (Ast.Bracket e)           = ("(" ^ (translateExpr e) ^ ")")

  | translateExpr (Ast.EVar  (x) )     = x 



  | translateExpr (Ast.ARVar  (x,e) )     = (x^"["^(translateExpr e)^"]")
  | translateExpr (Ast.EFncl (name,arg_list)) = (name^"("^translateArguments arg_list^")")
  
  | translateExpr (Ast.Op (x, oper, y))  = ((translateExpr x) ^ (Ast.binOpToString oper) ^ (translateExpr y ))
  | translateExpr (Ast.Erel (x, oper, y))  = ((translateExpr x) ^ (Ast.relOpToString oper) ^ (translateExpr y ))
  | translateExpr (Ast.Econd (x, oper, y))  = ((translateExpr x) ^ (Ast.condOpToString oper) ^ (translateExpr y ))

and translateArguments [x]     = translateExpr x
    |translateArguments (x::xs) = (translateExpr x ^" , "^ translateArguments xs)
    
    |translateArguments []      = ""





(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(***************************************************translate  CONDITION*****************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)



(*fun translateCondition (Ast.BConst Ast.TRUE)   = (" true ")

  | translateCondition (Ast.BConst Ast.FALSE)  = (" false ")

  | translateCondition (Ast.BVar x)             = (x)

  | translateCondition (Ast.CondOp (x,oper,y)) = ((translateCondition x) ^ (Ast.condOpToString oper) ^ (translateCondition y))

  | translateCondition (Ast.Rel (x,oper,y))    = ((translateExpr x)^ (Ast.relOpToString oper) ^ (translateExpr y))*)




(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(*****************************************translate STATEMENT AND STATEMENTS*************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)


fun translateStatement (Ast.As (x,exp,tp,isdef)) t    =
        let
          val tp_string =Atom.toString tp
        in
          if isdef then
          (addtabs t) ^ (x^" = ") ^ (translateExpr exp) ^ (";\n") 
          else
          (addtabs t) ^ ( tp_string^" "^x^" = ") ^ (translateExpr exp) ^ (";\n")  
        end
 

 | translateStatement (Ast.Ret exp)         t  = ( (addtabs t) ^ "return " ^(translateExpr exp)^ ";\n" )



 | translateStatement (Ast.FnCl (x,arg_list))  t    =  ( (addtabs t) ^  (x^"("^translateArguments arg_list^");\n")  )
 | translateStatement (Ast.If (c,sl))    t = (
              (addtabs t) ^ ("if(") ^ (translateExpr c) ^  ("){\n") ^
              (translateStatements (t+1,sl) ) ^
              (addtabs t) ^ ("}\n")
             ) 
 | translateStatement (Ast.IfEl (c,sl1,sl2)) t = (
              (addtabs t) ^  ("if(") ^ (translateExpr c) ^  ("){\n") ^

              (translateStatements (t+1,sl1)) ^

              (addtabs t) ^ ("}\n") ^

              (addtabs t) ^  ("else{\n") ^

              (translateStatements (t+1,sl2) )^

              (addtabs t )^ ("}\n")
             )
 | translateStatement (Ast.DirectC x)      t= 
     let
      fun not_dollar x = if Char.compare(x,#"$") = EQUAL then false else true 
       val remove_dollar = implode(List.filter not_dollar (explode x))

       val tabs = #"\n"::(explode(addtabs t))
       
       fun createtabs (x::xs) =
              ( case x of
                #"\n" => tabs@createtabs xs
                | x => x::createtabs xs
                )
          |createtabs [] = []

        val new_string = implode(createtabs (explode remove_dollar))

     in
       ((addtabs t)^new_string^"\n")
     end

 | translateStatement (Ast.StList ls) t     = (translateStatements (t,ls))

 | translateStatement (Ast.While (c,sl)) t  = (
              (addtabs t) ^ ("while(") ^ (translateExpr c) ^  ("){\n") ^
              (translateStatements (t+1,sl) ) ^
              (addtabs t) ^ ("}\n")
                                              )
 | translateStatement (Ast.EmptyStatement) t= ("")



and  translateStatements  (t,(x :: xs))   = ((translateStatement x t)^(translateStatements (t,xs)))
 |translateStatements  (t,[])        = ("")




(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(************************************************translate FUNCTION**********************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)

fun translateFun(Ast.Fun (x,g,tp,arg_list))  t  =  let
           val ret_type = Atom.toString tp
           in
            (
           (ret_type^" "^x^"("^translateArguments_f arg_list^"){\n")^
           (translateStatements  (t+1,g) )^
            ("}\n")
            
            )
           end





(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(***************************************************translate PROGRAM ELEMENT************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)

fun   translateElem (Ast.St statement)   = translateStatement statement 0 
 | translateElem (Ast.Fn function)       = (translateFun  function 0) 





(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(**************************************************translate*****************************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)

fun translate []        = ("")
  | translate (x :: xs) = ((translateElem x)^(translate xs))

end
