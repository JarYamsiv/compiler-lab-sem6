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


fun translateExpr (CAst.Const x )         = (" "^(Int.toString x)^" ")

  | translateExpr (CAst.EVar  x )     = x 

  | translateExpr (CAst.ARVar  (x,e) )     = (x^"["^(translateExpr e)^"]")
  
  | translateExpr (CAst.Op (x, oper, y))  = ((translateExpr x) ^ (CAst.binOpToString oper) ^ (translateExpr y ))





(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(***************************************************translate  CONDITION*****************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)



fun translateCondition (CAst.BConst CAst.TRUE)   = (" true ")

  | translateCondition (CAst.BConst CAst.FALSE)  = (" false ")

  | translateCondition (CAst.BVar x)             = (x)

  | translateCondition (CAst.CondOp (x,oper,y)) = ((translateCondition x) ^ (CAst.condOpToString oper) ^ (translateCondition y))

  | translateCondition (CAst.Rel (x,oper,y))    = ((translateExpr x)^ (CAst.relOpToString oper) ^ (translateExpr y))




(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(*****************************************translate STATEMENT AND STATEMENTS*************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)


fun translateStatement (CAst.As (x,exp,tp,isdef)) t    =
        let
          val tp_string = case tp of CAst.VOID => "void" | CAst.INT => "int" | CAst.BOOL => "uint8_t" | CAst.UNDEF => " "
        in
          if isdef then
          (addtabs t) ^ (x^" = ") ^ (translateExpr exp) ^ (";\n") 
          else
          (addtabs t) ^ (tp_string^" "^x^" = ") ^ (translateExpr exp) ^ (";\n")  
        end
 
 | translateStatement (CAst.BAs (x,c,isdef)) t = 
        let
          val tp_string = "uint8_t"
        in
          if isdef then
          (addtabs t) ^ (x^" = ") ^ (translateCondition c) ^ (";\n") 
          else
          (addtabs t) ^ (tp_string^" "^x^" = ") ^ (translateCondition c) ^ (";\n")  
        end

  | translateStatement (CAst.GAs(lhs,rhs))  t = ("")


 | translateStatement (CAst.Ret exp)         t  = ( (addtabs t) ^ "return " ^(translateExpr exp)^ ";\n" )



 | translateStatement (CAst.FnCl x)  t    =  ( (addtabs t) ^  (x^"();\n")  )
 | translateStatement (CAst.If (c,sl))    t = (
              (addtabs t) ^ ("if(") ^ (translateCondition c) ^  ("){\n") ^
              (translateStatements (t+1,sl) ) ^
              (addtabs t) ^ ("}\n")
             ) 
 | translateStatement (CAst.IfEl (c,sl1,sl2)) t = (
              (addtabs t) ^  ("if(") ^ (translateCondition c) ^  ("){\n") ^

              (translateStatements (t+1,sl1)) ^

              (addtabs t) ^ ("}\n") ^

              (addtabs t) ^  ("else{\n") ^

              (translateStatements (t+1,sl2) )^

              (addtabs t )^ ("}\n")
             )
 | translateStatement (CAst.DirectC x)      t= 
     let
      fun not_dollar x = if Char.compare(x,#"$") = EQUAL then false else true 
       val remove_dollar = implode(List.filter not_dollar (explode x))
     in
       (remove_dollar^"\n")
     end

 | translateStatement (CAst.StList ls) t     = (translateStatements (t,ls))

 | translateStatement (CAst.While (c,sl)) t  = (
              (addtabs t) ^ ("while(") ^ (translateCondition c) ^  ("){\n") ^
              (translateStatements (t+1,sl) ) ^
              (addtabs t) ^ ("}\n")
                                              )
 | translateStatement (CAst.EmptyStatement) t= ("")



and  translateStatements  (t,(x :: xs))   = ((translateStatement x t)^(translateStatements (t,xs)))
 |translateStatements  (t,[])        = ("")




(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(************************************************translate FUNCTION**********************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)

fun translateFun(CAst.Fun (x,g,tp))  t  =  let
           val ret_type = case tp of CAst.VOID => "void" | CAst.INT => "int" | CAst.BOOL => "uint8" | CAst.UNDEF=> "void"
           in
            (
           (ret_type^" "^x^"(){\n")^
           (translateStatements  (t+1,g) )^
            ("}\n")
            
            )
           end





(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(***************************************************translate PROGRAM ELEMENT************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)

fun   translateElem (CAst.St statement)   = translateStatement statement 0 
 | translateElem (CAst.Fn function)       = (translateFun  function 0) 





(**************************************************************************************************************************************)
(**************************************************************************************************************************************)
(**************************************************translate*****************************************************************************)
(**************************************************************************************************************************************)
(**************************************************************************************************************************************)

fun translate []        = ("")
  | translate (x :: xs) = ((translateElem x)^(translate xs))

end
