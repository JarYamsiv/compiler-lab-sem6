functor ExprLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Expr_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "expr.grm"*)(* This is the preamble where you can have arbitrary sml code. For us
it is empty *)


(*#line 15.1 "expr.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\004\000\000\000\
\\001\000\002\000\010\000\005\000\009\000\000\000\
\\001\000\003\000\006\000\000\000\
\\001\000\006\000\012\000\000\000\
\\015\000\000\000\
\\016\000\002\000\004\000\000\000\
\\017\000\000\000\
\\018\000\007\000\011\000\000\000\
\\019\000\000\000\
\\020\000\000\000\
\\021\000\002\000\010\000\000\000\
\\022\000\000\000\
\"
val actionRowNumbers =
"\001\000\006\000\003\000\005\000\
\\002\000\008\000\004\000\009\000\
\\011\000\010\000\007\000\012\000\
\\000\000"
val gotoT =
"\
\\001\000\012\000\002\000\001\000\000\000\
\\001\000\003\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\006\000\004\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 13
val numrules = 8
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | IDENTIFIER of  (string) | IDARR of  (Ast.Id list) | RHS of  (Ast.Rhs) | PROGRAMELEM of  (Ast.Rule) | PROGRAM of  (Ast.Rule list)
end
type svalue = MlyValue.svalue
type result = Ast.Rule list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "IDENTIFIER"
  | (T 2) => "COLON"
  | (T 3) => "BAR"
  | (T 4) => "EPS"
  | (T 5) => "SEMICOLON"
  | (T 6) => "EOP"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM, _, PROGRAM1right)) :: ( _, ( MlyValue.PROGRAMELEM PROGRAMELEM, PROGRAMELEM1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 44.38 "expr.grm"*)PROGRAMELEM::PROGRAM(*#line 159.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PROGRAMELEM1left, PROGRAM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.PROGRAMELEM PROGRAMELEM, PROGRAMELEM1left, PROGRAMELEM1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 45.38 "expr.grm"*)[PROGRAMELEM](*#line 163.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PROGRAMELEM1left, PROGRAMELEM1right), rest671)
end
|  ( 2, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.RHS RHS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAMELEM ((*#line 47.45 "expr.grm"*)Ast.Rul (IDENTIFIER,RHS)(*#line 167.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, IDENTIFIER1left, SEMICOLON1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.IDARR IDARR, IDARR1left, IDARR1right)) :: rest671)) => let val  result = MlyValue.RHS ((*#line 49.38 "expr.grm"*)Ast.Rh IDARR(*#line 171.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, IDARR1left, IDARR1right), rest671)
end
|  ( 4, ( ( _, ( _, EPS1left, EPS1right)) :: rest671)) => let val  result = MlyValue.RHS ((*#line 50.38 "expr.grm"*)Ast.Rh [(Ast.EPSILON)](*#line 175.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EPS1left, EPS1right), rest671)
end
|  ( 5, ( ( _, ( _, _, EOP1right)) :: ( _, ( MlyValue.IDARR IDARR, IDARR1left, _)) :: rest671)) => let val  result = MlyValue.RHS ((*#line 51.23 "expr.grm"*)Ast.Rh  (IDARR @ [Ast.EOP])  (*#line 179.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, IDARR1left, EOP1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.IDARR ((*#line 53.40 "expr.grm"*)[Ast.St (Atom.atom IDENTIFIER)](*#line 183.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.IDARR IDARR, _, IDARR1right)) :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.IDARR ((*#line 54.40 "expr.grm"*)(Ast.St (Atom.atom IDENTIFIER))::IDARR(*#line 187.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, IDENTIFIER1left, IDARR1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Expr_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.IDENTIFIER i,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun EPS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun EOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
end
end
