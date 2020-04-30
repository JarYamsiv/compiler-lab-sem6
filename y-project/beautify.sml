
val blue = "\027[1;34m"
val white = "\027[0m"
val yellow = "\027[0;33m"
val black   = "\027[0;30m"
val red = "\027[0;31m"
val green = "\027[0;32m"
val magenta = "\027[0;35m"
val cyan = "\027[0;36m"
structure beautify =
struct
  fun  add space = if (space = 0) then ("") else ("  "^(add (space-1)))

  fun indent space  n (Ast.NIL ) = "nil"
    | indent space  n (Ast.Const x) = (Int.toString x)
    | indent space  n (Ast.Quote x) = x
    | indent space  n (Ast.Array (a,b,c) ) = a^"["^( indent 0 n b)^"]"^" of "^( indent 0 n c)
    | indent space  n (Ast.Record (a, b) )  = 	let
						fun printRecbody [] 	= "" 
						  | printRecbody ((a,b)::[]) = a^" = "^( indent 0 n b)
						  | printRecbody ((a,b)::x )  = a^" = "^( indent 0 n b)^", "^(printRecbody x)		
					in
						a^"{"^(printRecbody b)^"}\n"	
					end	
    | indent space  n (Ast.Object a)       = (add space)^"new "^a
    | indent space  n (Ast.Name x) = x
    | indent space  n (Ast.Method(x,y) ) = ( indent 0 n x)^"."^y	
    | indent space  n (Ast.Access(x,y) ) = ( indent 0 n x)^"["^( indent 0 n y)^"]"
    | indent space  n (Ast.FunCall (a, b) )       = n^(add space)^a^"("^(Parguments  b)^")"
    | indent space  n (Ast.MethodCall(a, b, c))       = n^(add space)^(indent space  n a)^"."^b^"("^(Parguments  c)^")"
    | indent space  n (Ast.Neg x)   = "( ~"^( indent 0 n x)^")"
    | indent space  n (Ast.Op(a, oper, b)) = "("^( indent 0 n a)^(Ast.binOPtoString oper)^( indent 0 n b)^")"
    | indent space  n (Ast.Closed x) = "(\n"^(indentlist (space+1) n x)^"\n"^(add space)^")"
    | indent space  n  (Ast.Assign (x, y) ) = (add space) ^( indent 0 n x)^" := "^( indent 0 n y)
    | indent space  n (Ast.OPENIF (a,b) ) =n^(add space)^blue^"if "^white^" " ^ (indent space  n a) ^blue^" then "^white^(indent (space+1) n b) 
    | indent space  n (Ast.CLOSEDIF (a,b, c) ) = n^(indent space  n (Ast.OPENIF(a, b)))^(add space)^blue^" else "^white^(indent (space+1) n c)
    | indent space  n  (Ast.WHILE (x,y) )   = (add space)^blue^"while "^white^(indent space  n x)^blue^" do"^white^"\n"^(add (space+1))^(indent space  ("\n") y)
    | indent space  n  (Ast.FOR   (a, b, c, d) )= "\n"^(add space)^blue^"for "^white^a^" := "^( indent 0 n b)^blue^" to "^white^( indent 0 n c)^blue^" do "^white^(indent (space+1) ("\n") d)
    | indent space  n (Ast.BREAK) = (add space)^blue^"break"^white
    | indent space  n (Ast.LET(a, b) ) = (add space)^blue^"let"^white^"\n"^(indentdeclist (space+1) n a)^blue^"\n"^(add space)^"in"^white^"\n"^(indentlist (space+1) n b)^"\n"^(add space)^blue^"end"^white
   				
and
      indentdec space  n (Ast.VariableDec a) = (pvardec space  n a)
      |indentdec space  n (Ast.TypeDec (a, b) ) = (add space)^"type "^a^" = "^(printty 0 ("") b)
      |indentdec space  n (Ast.ClassDec (a,b) ) = (add space)^"class "^a^"{ \n"^(indentclasslist (space+1) n b)^(add space)^"}\n"
      |indentdec space  n(Ast.ClassDecType (a,b,c) ) = (add space)^"class "^a^" extends "^b^"{ \n"^(indentclasslist (space+1) n c)^(add space)^"}\n"
      |indentdec space  n (Ast.Import a) = (add space)^"import "^a^" \n "
      |indentdec space  n (Ast.FunctionDec (a,b,c)) = (add space)^"function "^a^"("^(ptyfield b)^") = "^(indent space  n c)^" \n "
      |indentdec space  n (Ast.FunctionDecType (a,b,c,d)) = (add space)^"function "^a^"("^(ptyfield b)^"): "^c^ " = "^(indent space  n d)^" \n "
      |indentdec space  n (Ast.PrimitiveDec (a,b)) = (add space)^"primitive "^a^"("^(ptyfield b)^")\n "
      |indentdec space  n (Ast.PrimitiveDecType (a,b,c)) = (add space)^"primitive "^a^"("^(ptyfield b)^"): "^c^"\n"
and 
	Parguments [] = ""
	|Parguments (x::[]) = ( indent 0 ("") x)^""
	|Parguments (x::xs) = ( indent 0 ("") x)^", "^(Parguments xs)

and	ptyfield (Ast.Tyfield a) = let
						fun p [] = " "
						|p ((a,b)::[]) = a^" : "^b
						|p ((a,b)::xs) = a^" : "^b^" , "^(p xs)
					in
						(p a)
					end
and
	printty s  n (Ast.NameTy a) 		= a
	|printty s  n (Ast.RecordTy a) 	= "{"^(ptyfield a)^"}"
	|printty s  n (Ast.ArrayTy a) 	= "array of "^a
	|printty s  n (Ast.ClassDefCan a)	= "class { \n"^(indentclasslist (s+1) n a)^(add s)^"}\n"
	|printty s  n (Ast.ClassDefCanType (a,b) ) = "class  extends "^a^"{ \n"^(indentclasslist (s+1) n b)^(add s)^"}\n"
and
      indentdeclist s  n []      = ""
     |indentdeclist s  n (x::xs) = (indentdec s  n x)^"\n"^(indentdeclist s  n xs)
and 
      indentlist s  n []      = "\n"
     |indentlist s  n (x::[]) = n^(indent s  n x)
     |indentlist s  n (x::xs) = n^(indent s  n x)^";"^(indentlist s  ("\n") xs)
  and
      classfield s  n (Ast.MethodDec (a,b,c) ) = (add s)^"method "^a^" ( "^(ptyfield  b)^" ) = "^( indent 0 n c)  
     | classfield s  n (Ast.MethodDecType (a,b,c,d) ) = (add s)^"method "^a^"( "^(ptyfield b)^" ) : "^c^" = "^( indent 0 n d)
     | classfield s  n (Ast.ClassAttribute a) = (pvardec s  n a)    

and
	indentclasslist s  n []      = ""
     |indentclasslist s  n (x::xs) = (classfield s  n x)^"\n"^(indentclasslist s  n xs)
and
	pvardec s  n (Ast.VarDec(a, b)) = (add s)^"var "^a^" := "^(indent s  n b)
	|pvardec s  n (Ast.VarDecType(a, b,c)) = (add s)^"var "^a^": "^b^" := "^(indent s  n c)
and
  pretty s  n (Ast.Foo a)  = (indent s  n a)^"\n"
 |pretty s  n (Ast.Bar a) = (indentdeclist s  n a)

     
end
