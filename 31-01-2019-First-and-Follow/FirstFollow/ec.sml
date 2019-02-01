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
val rule_map  = Translate.compile program AtomMap.empty
val _ = Translate.printmap rule_map


end
