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




(*#line 17.1 "expr.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\016\000\008\000\015\000\014\000\014\000\000\000\
\\001\000\002\000\005\000\006\000\035\000\008\000\049\000\018\000\049\000\000\000\
\\001\000\006\000\000\000\000\000\
\\001\000\008\000\007\000\018\000\006\000\000\000\
\\001\000\008\000\018\000\000\000\
\\001\000\011\000\031\000\000\000\
\\001\000\014\000\010\000\000\000\
\\001\000\015\000\017\000\000\000\
\\001\000\015\000\024\000\020\000\023\000\021\000\022\000\022\000\021\000\
\\023\000\020\000\024\000\019\000\000\000\
\\001\000\015\000\032\000\020\000\023\000\021\000\022\000\022\000\021\000\
\\023\000\020\000\024\000\019\000\000\000\
\\034\000\000\000\
\\036\000\009\000\012\000\014\000\011\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\023\000\020\000\024\000\019\000\000\000\
\\043\000\023\000\020\000\024\000\019\000\000\000\
\\044\000\023\000\020\000\024\000\019\000\000\000\
\\045\000\000\000\
\\046\000\023\000\020\000\000\000\
\\047\000\000\000\
\\048\000\000\000\
\\049\000\002\000\005\000\000\000\
\"
val actionRowNumbers =
"\024\000\003\000\001\000\024\000\
\\006\000\011\000\010\000\023\000\
\\000\000\007\000\004\000\008\000\
\\000\000\016\000\015\000\012\000\
\\013\000\000\000\000\000\000\000\
\\000\000\000\000\005\000\009\000\
\\021\000\020\000\018\000\019\000\
\\017\000\014\000\022\000\002\000"
val gotoT =
"\
\\001\000\031\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\001\000\006\000\002\000\002\000\003\000\001\000\000\000\
\\003\000\007\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\024\000\000\000\
\\004\000\025\000\000\000\
\\004\000\026\000\000\000\
\\004\000\027\000\000\000\
\\004\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 32
val numrules = 16
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
datatype svalue = VOID | ntVOID of unit | IDENTIFIER of  (string) | CONST of  (int) | CONDITION of  (Ast.Condition) | INDENT of  (int) | STATEMENT of  (Ast.Statement) | PROGRAM of  (Ast.Statement list)
end
type svalue = MlyValue.svalue
type result = Ast.Statement list
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
fn (T 5) => true | _ => false
val showTerminal =
fn (T 0) => "CONST"
  | (T 1) => "TAB"
  | (T 2) => "PLUS"
  | (T 3) => "MINUS"
  | (T 4) => "MUL"
  | (T 5) => "EOF"
  | (T 6) => "NEWLINE"
  | (T 7) => "IDENTIFIER"
  | (T 8) => "EQUALSIGN"
  | (T 9) => "SEMICOLON"
  | (T 10) => "COLON"
  | (T 11) => "LCURL"
  | (T 12) => "RCURL"
  | (T 13) => "LPAREN"
  | (T 14) => "RPAREN"
  | (T 15) => "LSQUARE"
  | (T 16) => "RSQUARE"
  | (T 17) => "IF"
  | (T 18) => "ELSE"
  | (T 19) => "EQ"
  | (T 20) => "GT"
  | (T 21) => "LT"
  | (T 22) => "AND"
  | (T 23) => "OR"
  | (T 24) => "FUN"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM, _, PROGRAM1right)) :: ( _, ( MlyValue.STATEMENT STATEMENT, STATEMENT1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 60.37 "expr.grm"*)STATEMENT::PROGRAM(*#line 216.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, STATEMENT1left, PROGRAM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.STATEMENT STATEMENT, STATEMENT1left, STATEMENT1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 61.37 "expr.grm"*)[STATEMENT](*#line 220.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, STATEMENT1left, STATEMENT1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, IDENTIFIER1right)) :: ( _, ( MlyValue.INDENT INDENT, INDENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 64.14 "expr.grm"*)Ast.Ident (INDENT,IDENTIFIER)(*#line 224.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, INDENT1left, IDENTIFIER1right), rest671)
end
|  ( 3, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( MlyValue.INDENT INDENT, INDENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 67.14 "expr.grm"*)Ast.FunCall (INDENT,IDENTIFIER)(*#line 228.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, INDENT1left, RPAREN1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER2, _, IDENTIFIER2right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( MlyValue.INDENT INDENT, INDENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 70.14 "expr.grm"*)Ast.Assignment (INDENT,IDENTIFIER1,IDENTIFIER2)(*#line 232.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, INDENT1left, IDENTIFIER2right), rest671)
end
|  ( 5, ( ( _, ( _, _, COLON1right)) :: _ :: _ :: _ :: _ :: ( _, ( MlyValue.INDENT INDENT, INDENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 73.14 "expr.grm"*)Ast.Empty INDENT(*#line 236.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, INDENT1left, COLON1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.CONDITION ((*#line 77.42 "expr.grm"*) Ast.CConst CONST(*#line 240.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, CONST1left, CONST1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.CONDITION ((*#line 78.42 "expr.grm"*) Ast.CVar IDENTIFIER(*#line 244.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.CONDITION CONDITION2, _, CONDITION2right)) :: _ :: ( _, ( MlyValue.CONDITION CONDITION1, CONDITION1left, _)) :: rest671)) => let val  result = MlyValue.CONDITION ((*#line 79.42 "expr.grm"*) Ast.eq CONDITION1 CONDITION2(*#line 248.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, CONDITION1left, CONDITION2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.CONDITION CONDITION2, _, CONDITION2right)) :: _ :: ( _, ( MlyValue.CONDITION CONDITION1, CONDITION1left, _)) :: rest671)) => let val  result = MlyValue.CONDITION ((*#line 80.42 "expr.grm"*) Ast.lt CONDITION1 CONDITION2(*#line 252.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, CONDITION1left, CONDITION2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.CONDITION CONDITION2, _, CONDITION2right)) :: _ :: ( _, ( MlyValue.CONDITION CONDITION1, CONDITION1left, _)) :: rest671)) => let val  result = MlyValue.CONDITION ((*#line 81.42 "expr.grm"*) Ast.gt CONDITION1 CONDITION2(*#line 256.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, CONDITION1left, CONDITION2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.CONDITION CONDITION2, _, CONDITION2right)) :: _ :: ( _, ( MlyValue.CONDITION CONDITION1, CONDITION1left, _)) :: rest671)) => let val  result = MlyValue.CONDITION ((*#line 82.42 "expr.grm"*) Ast.nd CONDITION1 CONDITION2(*#line 260.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, CONDITION1left, CONDITION2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.CONDITION CONDITION2, _, CONDITION2right)) :: _ :: ( _, ( MlyValue.CONDITION CONDITION1, CONDITION1left, _)) :: rest671)) => let val  result = MlyValue.CONDITION ((*#line 83.42 "expr.grm"*) Ast.or CONDITION1 CONDITION2(*#line 264.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, CONDITION1left, CONDITION2right), rest671)
end
|  ( 13, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.CONDITION CONDITION, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.CONDITION ((*#line 84.42 "expr.grm"*) CONDITION (*#line 268.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.INDENT INDENT, _, INDENT1right)) :: ( _, ( _, TAB1left, _)) :: rest671)) => let val  result = MlyValue.INDENT ((*#line 87.37 "expr.grm"*)INDENT+1(*#line 272.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, TAB1left, INDENT1right), rest671)
end
|  ( 15, ( rest671)) => let val  result = MlyValue.INDENT ((*#line 88.37 "expr.grm"*)0(*#line 276.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
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
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.CONST i,p1,p2))
fun TAB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.IDENTIFIER i,p1,p2))
fun EQUALSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun LCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun RCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun LSQUARE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun RSQUARE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
end
end
