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
\\001\000\001\000\014\000\013\000\013\000\000\000\
\\001\000\001\000\036\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\014\000\024\000\000\000\
\\001\000\005\000\000\000\000\000\
\\001\000\007\000\006\000\017\000\029\000\000\000\
\\001\000\007\000\006\000\024\000\005\000\000\000\
\\001\000\007\000\009\000\000\000\
\\001\000\009\000\007\000\000\000\
\\001\000\009\000\032\000\000\000\
\\001\000\011\000\020\000\000\000\
\\001\000\011\000\040\000\000\000\
\\001\000\011\000\044\000\000\000\
\\001\000\012\000\030\000\000\000\
\\001\000\012\000\042\000\000\000\
\\001\000\012\000\046\000\000\000\
\\001\000\013\000\011\000\000\000\
\\001\000\013\000\033\000\000\000\
\\001\000\014\000\015\000\000\000\
\\001\000\014\000\038\000\019\000\037\000\000\000\
\\048\000\000\000\
\\049\000\007\000\006\000\024\000\005\000\000\000\
\\050\000\008\000\010\000\000\000\
\\051\000\002\000\018\000\003\000\017\000\004\000\016\000\000\000\
\\052\000\007\000\006\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\\055\000\018\000\043\000\000\000\
\\056\000\000\000\
\\057\000\000\000\
\\058\000\007\000\006\000\017\000\029\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\019\000\037\000\000\000\
\\063\000\000\000\
\\064\000\004\000\016\000\000\000\
\\065\000\000\000\
\\066\000\004\000\016\000\000\000\
\\067\000\000\000\
\"
val actionRowNumbers =
"\005\000\007\000\020\000\006\000\
\\021\000\030\000\019\000\015\000\
\\000\000\017\000\022\000\000\000\
\\034\000\009\000\000\000\000\000\
\\000\000\002\000\004\000\038\000\
\\037\000\035\000\036\000\012\000\
\\029\000\025\000\008\000\016\000\
\\031\000\028\000\023\000\001\000\
\\024\000\018\000\032\000\001\000\
\\010\000\033\000\004\000\013\000\
\\026\000\011\000\004\000\014\000\
\\027\000\003\000"
val gotoT =
"\
\\002\000\045\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\002\000\006\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\010\000\000\000\
\\000\000\
\\000\000\
\\001\000\017\000\000\000\
\\000\000\
\\000\000\
\\001\000\019\000\000\000\
\\001\000\020\000\000\000\
\\001\000\021\000\000\000\
\\000\000\
\\004\000\026\000\005\000\025\000\006\000\024\000\007\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\026\000\005\000\025\000\006\000\024\000\007\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\026\000\005\000\032\000\000\000\
\\008\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\037\000\000\000\
\\000\000\
\\000\000\
\\004\000\026\000\005\000\025\000\006\000\024\000\007\000\039\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\026\000\005\000\025\000\006\000\024\000\007\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 46
val numrules = 20
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
datatype svalue = VOID | ntVOID of unit | IDENTIFIER of  (string) | CONST of  (int) | CONDITION of  (Ast.Condition) | CODEBLOCKS of  (Ast.CodeBlock list) | CODEBLOCK of  (Ast.CodeBlock) | STATEMENTS of  (Ast.Statement list) | STATEMENT of  (Ast.Statement) | PROGRAMELEM of  (Ast.ProgramElement) | PROGRAM of  (Ast.ProgramElement list) | EXP of  (Ast.Expr)
end
type svalue = MlyValue.svalue
type result = Ast.ProgramElement list
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
fn (T 4) => true | _ => false
val showTerminal =
fn (T 0) => "CONST"
  | (T 1) => "PLUS"
  | (T 2) => "MINUS"
  | (T 3) => "MUL"
  | (T 4) => "EOF"
  | (T 5) => "NEWLINE"
  | (T 6) => "IDENTIFIER"
  | (T 7) => "EQUALSIGN"
  | (T 8) => "SEMICOLON"
  | (T 9) => "COLON"
  | (T 10) => "LCURL"
  | (T 11) => "RCURL"
  | (T 12) => "LPAREN"
  | (T 13) => "RPAREN"
  | (T 14) => "LSQUARE"
  | (T 15) => "RSQUARE"
  | (T 16) => "IF"
  | (T 17) => "ELSE"
  | (T 18) => "EQ"
  | (T 19) => "GT"
  | (T 20) => "LT"
  | (T 21) => "AND"
  | (T 22) => "OR"
  | (T 23) => "FUN"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM, _, PROGRAM1right)) :: ( _, ( MlyValue.PROGRAMELEM PROGRAMELEM, PROGRAMELEM1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 55.35 "expr.grm"*)PROGRAMELEM::PROGRAM(*#line 243.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, PROGRAMELEM1left, PROGRAM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.PROGRAMELEM PROGRAMELEM, PROGRAMELEM1left, PROGRAMELEM1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 56.36 "expr.grm"*)[PROGRAMELEM](*#line 247.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, PROGRAMELEM1left, PROGRAMELEM1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 58.40 "expr.grm"*)Ast.Id IDENTIFIER(*#line 251.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 59.39 "expr.grm"*)Ast.As (IDENTIFIER,EXP)(*#line 255.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, IDENTIFIER1left, EXP1right), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.STATEMENT STATEMENT, STATEMENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENTS ((*#line 61.45 "expr.grm"*)[STATEMENT](*#line 259.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, STATEMENT1left, SEMICOLON1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.STATEMENTS STATEMENTS, _, STATEMENTS1right)) :: _ :: ( _, ( MlyValue.STATEMENT STATEMENT, STATEMENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENTS ((*#line 62.45 "expr.grm"*)STATEMENT::STATEMENTS(*#line 263.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, STATEMENT1left, STATEMENTS1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.STATEMENTS STATEMENTS, STATEMENTS1left, STATEMENTS1right)) :: rest671)) => let val  result = MlyValue.CODEBLOCK ((*#line 64.39 "expr.grm"*)Ast.Stlist STATEMENTS(*#line 267.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, STATEMENTS1left, STATEMENTS1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RCURL1right)) :: ( _, ( MlyValue.CODEBLOCKS CODEBLOCKS, _, _)) :: _ :: _ :: ( _, ( MlyValue.CONDITION CONDITION, _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.CODEBLOCK ((*#line 65.63 "expr.grm"*)Ast.Cblock (CONDITION,CODEBLOCKS)(*#line 271.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, IF1left, RCURL1right), rest671)
end
|  ( 8, ( ( _, ( _, _, RCURL2right)) :: _ :: _ :: _ :: _ :: ( _, ( MlyValue.CODEBLOCKS CODEBLOCKS1, _, _)) :: _ :: _ :: ( _, ( MlyValue.CONDITION CONDITION, _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.CODEBLOCK ((*#line 66.91 "expr.grm"*)Ast.Cblock (CONDITION,CODEBLOCKS1)(*#line 275.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, IF1left, RCURL2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.CODEBLOCKS CODEBLOCKS, _, CODEBLOCKS1right)) :: ( _, ( MlyValue.CODEBLOCK CODEBLOCK, CODEBLOCK1left, _)) :: rest671)) => let val  result = MlyValue.CODEBLOCKS ((*#line 68.37 "expr.grm"*)CODEBLOCK::CODEBLOCKS(*#line 279.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, CODEBLOCK1left, CODEBLOCKS1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.CODEBLOCK CODEBLOCK, CODEBLOCK1left, CODEBLOCK1right)) :: rest671)) => let val  result = MlyValue.CODEBLOCKS ((*#line 69.38 "expr.grm"*)[CODEBLOCK](*#line 283.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, CODEBLOCK1left, CODEBLOCK1right), rest671)
end
|  ( 11, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.STATEMENT STATEMENT, STATEMENT1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAMELEM ((*#line 73.37 "expr.grm"*)Ast.St STATEMENT(*#line 287.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, STATEMENT1left, SEMICOLON1right), rest671)
end
|  ( 12, ( ( _, ( _, _, RCURL1right)) :: ( _, ( MlyValue.CODEBLOCKS CODEBLOCKS, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAMELEM ((*#line 76.19 "expr.grm"*)Ast.Fn (Ast.Fun (IDENTIFIER,CODEBLOCKS))(*#line 291.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, FUN1left, RCURL1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.CONDITION ((*#line 78.42 "expr.grm"*) Ast.CConst CONST(*#line 295.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, CONST1left, CONST1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.CONDITION CONDITION2, _, CONDITION2right)) :: _ :: ( _, ( MlyValue.CONDITION CONDITION1, CONDITION1left, _)) :: rest671)) => let val  result = MlyValue.CONDITION ((*#line 79.42 "expr.grm"*) Ast.eq CONDITION1 CONDITION2(*#line 299.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, CONDITION1left, CONDITION2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 82.33 "expr.grm"*) Ast.Const CONST     (*#line 303.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 83.33 "expr.grm"*) Ast.plus  EXP1 EXP2 (*#line 307.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 84.33 "expr.grm"*) EXP (*#line 311.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 85.32 "expr.grm"*) Ast.minus EXP1 EXP2 (*#line 315.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 86.32 "expr.grm"*) Ast.mul   EXP1 EXP2 (*#line 319.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
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
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.IDENTIFIER i,p1,p2))
fun EQUALSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun LCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun RCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun LSQUARE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun RSQUARE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
end
end
