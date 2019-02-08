(* The compiler from expression to rp *)
structure EC =
struct

(* This three structure definitions are what the lexer and parser *)

structure ExprLrVals = ExprLrValsFun(structure Token = LrParser.Token) (* Generate the LR values structure *)
structure ExprLex    = ExprLexFun(structure Tokens = ExprLrVals.Tokens)
structure ExprParser = Join( structure ParserData = ExprLrVals.ParserData
			     structure Lex        = ExprLex
			     structure LrParser   = LrParser
			   )

(* Build Lexers *)
fun makeExprLexer strm = ExprParser.makeLexer (fn n => TextIO.inputN(strm,n))

val makeFileLexer      = makeExprLexer o TextIO.openIn


(* Parse command line and set a suitable lexer *)

val thisLexer = case CommandLine.arguments() of
		    []  => (TextIO.output(TextIO.stdErr, "No File provided\n"); OS.Process.exit OS.Process.failure)
		 |  [x] => makeFileLexer x
		 |  _   => (TextIO.output(TextIO.stdErr, "usage: ec file\n"); OS.Process.exit OS.Process.failure)



fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

(* The portion of the code that does the actual compiling *)

val (program,_) = ExprParser.parse (0,thisLexer,print_error,())
val (rule_map,sym_table,tok_table)  = Translate.compile program AtomMap.empty AtomSet.empty AtomSet.empty
val _ = print ("compiled productions : \n")
val _ = Translate.printmap (rule_map,sym_table,tok_table)

val nullable_set = Translate.calc_nullable(rule_map,sym_table,tok_table)
val first_map = Translate.calc_first(rule_map,sym_table,tok_table,nullable_set)
val follow_map = Translate.calc_follow(rule_map,sym_table,tok_table,nullable_set,first_map)
val lli_table = Translate.calc_lli_table(rule_map,sym_table,tok_table,nullable_set,first_map,follow_map)


end
