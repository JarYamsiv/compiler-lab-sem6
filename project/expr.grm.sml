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
\\001\000\001\000\029\000\009\000\028\000\016\000\027\000\023\000\026\000\
\\024\000\025\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\017\000\058\000\
\\025\000\038\000\026\000\037\000\027\000\036\000\028\000\035\000\
\\029\000\034\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\019\000\065\000\
\\025\000\038\000\026\000\037\000\027\000\036\000\028\000\035\000\
\\029\000\034\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\025\000\038\000\
\\026\000\037\000\027\000\036\000\028\000\035\000\029\000\034\000\
\\032\000\045\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\025\000\038\000\
\\026\000\037\000\027\000\036\000\028\000\035\000\029\000\034\000\
\\032\000\046\000\000\000\
\\001\000\005\000\000\000\000\000\
\\001\000\009\000\007\000\000\000\
\\001\000\011\000\033\000\016\000\032\000\000\000\
\\001\000\016\000\008\000\000\000\
\\001\000\017\000\011\000\000\000\
\\001\000\017\000\063\000\000\000\
\\001\000\017\000\066\000\000\000\
\\001\000\030\000\005\000\000\000\
\\001\000\032\000\013\000\000\000\
\\001\000\032\000\071\000\000\000\
\\001\000\033\000\022\000\000\000\
\\001\000\033\000\067\000\000\000\
\\001\000\033\000\068\000\000\000\
\\001\000\033\000\073\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\030\000\005\000\000\000\
\\078\000\002\000\041\000\003\000\040\000\004\000\039\000\025\000\038\000\
\\026\000\037\000\027\000\036\000\028\000\035\000\029\000\034\000\000\000\
\\079\000\000\000\
\\080\000\002\000\041\000\003\000\040\000\004\000\039\000\025\000\038\000\
\\026\000\037\000\027\000\036\000\028\000\035\000\029\000\034\000\000\000\
\\081\000\000\000\
\\082\000\021\000\070\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\009\000\021\000\010\000\020\000\020\000\019\000\022\000\018\000\
\\031\000\017\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\016\000\044\000\018\000\043\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\004\000\039\000\000\000\
\\095\000\000\000\
\\096\000\004\000\039\000\000\000\
\\097\000\000\000\
\\098\000\002\000\041\000\003\000\040\000\004\000\039\000\000\000\
\\099\000\002\000\041\000\003\000\040\000\004\000\039\000\000\000\
\\100\000\002\000\041\000\003\000\040\000\004\000\039\000\000\000\
\\101\000\002\000\041\000\003\000\040\000\004\000\039\000\025\000\038\000\
\\026\000\037\000\027\000\036\000\029\000\034\000\000\000\
\\102\000\002\000\041\000\003\000\040\000\004\000\039\000\025\000\038\000\
\\026\000\037\000\027\000\036\000\000\000\
\\103\000\000\000\
\\104\000\007\000\012\000\000\000\
\\105\000\009\000\010\000\000\000\
\\106\000\000\000\
\\107\000\002\000\041\000\003\000\040\000\004\000\039\000\007\000\064\000\
\\025\000\038\000\026\000\037\000\027\000\036\000\028\000\035\000\
\\029\000\034\000\000\000\
\\108\000\001\000\029\000\009\000\028\000\016\000\027\000\023\000\026\000\
\\024\000\025\000\000\000\
\"
val actionRowNumbers =
"\012\000\021\000\019\000\006\000\
\\020\000\008\000\049\000\009\000\
\\048\000\013\000\049\000\029\000\
\\047\000\015\000\029\000\000\000\
\\000\000\000\000\025\000\007\000\
\\031\000\030\000\024\000\034\000\
\\033\000\000\000\035\000\032\000\
\\003\000\004\000\052\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\001\000\000\000\052\000\029\000\
\\029\000\010\000\051\000\022\000\
\\046\000\045\000\042\000\043\000\
\\044\000\041\000\040\000\038\000\
\\039\000\002\000\011\000\016\000\
\\017\000\023\000\052\000\036\000\
\\037\000\028\000\026\000\050\000\
\\014\000\029\000\018\000\027\000\
\\005\000"
val gotoT =
"\
\\002\000\072\000\003\000\002\000\004\000\001\000\000\000\
\\003\000\004\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\007\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\012\000\000\000\
\\005\000\014\000\006\000\013\000\000\000\
\\000\000\
\\000\000\
\\005\000\014\000\006\000\021\000\000\000\
\\001\000\022\000\000\000\
\\001\000\028\000\000\000\
\\001\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\040\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\046\000\008\000\045\000\000\000\
\\001\000\047\000\000\000\
\\001\000\048\000\000\000\
\\001\000\049\000\000\000\
\\001\000\050\000\000\000\
\\001\000\051\000\000\000\
\\001\000\052\000\000\000\
\\001\000\053\000\000\000\
\\001\000\054\000\000\000\
\\001\000\055\000\000\000\
\\000\000\
\\001\000\057\000\000\000\
\\001\000\046\000\008\000\058\000\000\000\
\\005\000\014\000\006\000\059\000\000\000\
\\005\000\014\000\006\000\060\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\046\000\008\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\014\000\006\000\070\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 73
val numrules = 34
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
datatype svalue = VOID | ntVOID of unit | DIRECTC of  (string) | IDENTIFIER of  (string) | CONST of  (int) | CALLARGUMENTS of  (Ast.Expr list) | ARGUMENTS of  (Ast.Argument list) | STATEMENTS of  (Ast.Statement list) | STATEMENT of  (Ast.Statement) | PROGRAMELEM of  (Ast.ProgramElement) | PROGRAM of  (Ast.ProgramElement list) | START of  (Ast.ProgramElement list) | EXP of  (Ast.Expr)
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
  | (T 6) => "COMMA"
  | (T 7) => "BACKTICK"
  | (T 8) => "IDENTIFIER"
  | (T 9) => "DIRECTC"
  | (T 10) => "EQUALSIGN"
  | (T 11) => "SEMICOLON"
  | (T 12) => "COLON"
  | (T 13) => "LCURL"
  | (T 14) => "RCURL"
  | (T 15) => "LPAREN"
  | (T 16) => "RPAREN"
  | (T 17) => "LSQUARE"
  | (T 18) => "RSQUARE"
  | (T 19) => "IF"
  | (T 20) => "ELSE"
  | (T 21) => "WHILE"
  | (T 22) => "TRUE"
  | (T 23) => "FALSE"
  | (T 24) => "EQ"
  | (T 25) => "GT"
  | (T 26) => "LT"
  | (T 27) => "AND"
  | (T 28) => "OR"
  | (T 29) => "FUN"
  | (T 30) => "RET"
  | (T 31) => "BEG"
  | (T 32) => "END"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM, PROGRAM1left, PROGRAM1right)) :: rest671)) => let val  result = MlyValue.START ((*#line 72.35 "expr.grm"*)PROGRAM(*#line 317.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, PROGRAM1left, PROGRAM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.PROGRAM PROGRAM, _, PROGRAM1right)) :: ( _, ( MlyValue.PROGRAMELEM PROGRAMELEM, PROGRAMELEM1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 74.35 "expr.grm"*)PROGRAMELEM::PROGRAM(*#line 321.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, PROGRAMELEM1left, PROGRAM1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.PROGRAMELEM PROGRAMELEM, PROGRAMELEM1left, PROGRAMELEM1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 75.36 "expr.grm"*)[PROGRAMELEM](*#line 325.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, PROGRAMELEM1left, PROGRAMELEM1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 77.38 "expr.grm"*)Ast.As (IDENTIFIER,EXP,Atom.atom "undef",false)(*#line 329.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, IDENTIFIER1left, EXP1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.CALLARGUMENTS CALLARGUMENTS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 78.53 "expr.grm"*)Ast.FnCl (IDENTIFIER,CALLARGUMENTS)(*#line 333.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, IDENTIFIER1left, RPAREN1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( _, RET1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 79.40 "expr.grm"*)Ast.Ret EXP(*#line 337.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, RET1left, EXP1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.DIRECTC DIRECTC, DIRECTC1left, DIRECTC1right)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 80.40 "expr.grm"*)Ast.DirectC DIRECTC(*#line 341.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, DIRECTC1left, DIRECTC1right), rest671)
end
|  ( 7, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.STATEMENTS STATEMENTS, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 81.41 "expr.grm"*)Ast.If (EXP,STATEMENTS)(*#line 345.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, IF1left, END1right), rest671)
end
|  ( 8, ( ( _, ( _, _, END2right)) :: ( _, ( MlyValue.STATEMENTS STATEMENTS2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.STATEMENTS STATEMENTS1, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 82.65 "expr.grm"*)Ast.IfEl (EXP,STATEMENTS1,STATEMENTS2)(*#line 349.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, IF1left, END2right), rest671)
end
|  ( 9, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.STATEMENTS STATEMENTS, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 83.42 "expr.grm"*)Ast.While (EXP,STATEMENTS)(*#line 353.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, WHILE1left, END1right), rest671)
end
|  ( 10, ( rest671)) => let val  result = MlyValue.STATEMENTS ((*#line 86.45 "expr.grm"*)[](*#line 357.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 11, ( ( _, ( MlyValue.STATEMENTS STATEMENTS, _, STATEMENTS1right)) :: ( _, ( MlyValue.STATEMENT STATEMENT, STATEMENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENTS ((*#line 87.35 "expr.grm"*)STATEMENT::STATEMENTS(*#line 361.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, STATEMENT1left, STATEMENTS1right), rest671)
end
|  ( 12, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.STATEMENTS STATEMENTS, _, _)) :: _ :: _ :: ( _, ( MlyValue.ARGUMENTS ARGUMENTS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAMELEM ((*#line 97.17 "expr.grm"*)Ast.Fn (Ast.Fun (IDENTIFIER,STATEMENTS,Atom.atom "void",ARGUMENTS))(*#line 365.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, FUN1left, END1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 110.33 "expr.grm"*) Ast.Const (CONST)     (*#line 369.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 14, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 111.33 "expr.grm"*) Ast.BVal Ast.TRUE(*#line 373.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 15, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 112.34 "expr.grm"*) Ast.BVal Ast.FALSE(*#line 377.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 113.33 "expr.grm"*) Ast.EVar (IDENTIFIER) (*#line 381.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RSQUARE1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 114.43 "expr.grm"*)Ast.ARVar (IDENTIFIER,EXP)(*#line 385.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, RSQUARE1right), rest671)
end
|  ( 18, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.CALLARGUMENTS CALLARGUMENTS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 115.51 "expr.grm"*)Ast.EFncl(IDENTIFIER,CALLARGUMENTS)(*#line 389.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, RPAREN1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 116.33 "expr.grm"*) Ast.plus  EXP1 EXP2 (*#line 393.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 117.33 "expr.grm"*) Ast.Bracket EXP (*#line 397.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 118.32 "expr.grm"*) Ast.minus EXP1 EXP2 (*#line 401.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 119.32 "expr.grm"*) Ast.mul   EXP1 EXP2 (*#line 405.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 120.33 "expr.grm"*) Ast.lt EXP1 EXP2(*#line 409.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 121.33 "expr.grm"*) Ast.gt EXP1 EXP2(*#line 413.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 122.33 "expr.grm"*) Ast.eq EXP1 EXP2(*#line 417.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 123.34 "expr.grm"*) Ast.nd EXP1 EXP2(*#line 421.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 124.33 "expr.grm"*) Ast.or EXP1 EXP2(*#line 425.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ARGUMENTS ARGUMENTS, _, ARGUMENTS1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 126.41 "expr.grm"*)Ast.Arg(IDENTIFIER,Atom.atom "undef")::ARGUMENTS(*#line 429.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, IDENTIFIER1left, ARGUMENTS1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 127.41 "expr.grm"*)[Ast.Arg(IDENTIFIER,Atom.atom "undef")](*#line 433.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 30, ( rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 128.36 "expr.grm"*)[](*#line 437.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 31, ( ( _, ( MlyValue.CALLARGUMENTS CALLARGUMENTS, _, CALLARGUMENTS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.CALLARGUMENTS ((*#line 130.42 "expr.grm"*)EXP::CALLARGUMENTS(*#line 441.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, EXP1left, CALLARGUMENTS1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.CALLARGUMENTS ((*#line 131.34 "expr.grm"*)[EXP](*#line 445.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, EXP1left, EXP1right), rest671)
end
|  ( 33, ( rest671)) => let val  result = MlyValue.CALLARGUMENTS ((*#line 132.36 "expr.grm"*)[](*#line 449.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
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
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun BACKTICK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.IDENTIFIER i,p1,p2))
fun DIRECTC (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.DIRECTC i,p1,p2))
fun EQUALSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun LCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun RCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun LSQUARE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun RSQUARE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun RET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun BEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
end
end
