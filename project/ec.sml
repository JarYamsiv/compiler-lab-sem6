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
val inFile = ref ""
val outFile = ref ""

fun readCommandLine (comLine) = 
	let
		fun  head (x::xs) = SOME x
			|head []	  = NONE

		fun find_after (x) (y::ys) = if String.compare(x,y) = EQUAL then (head ys) else (find_after x ys)
			|find_after (x)	[]	   = NONE

		fun convert x = (case (List.getItem x) of
						SOME (x,y) =>(
								case (find_after "-o" y) of
									SOME z => SOME (x,z)
									| NONE => NONE
							)
						|NONE => NONE
			)
	in
		case comLine of
			(x::xs) => ( case (convert (x::xs)) of
						SOME (x,y) => (inFile := x; outFile := y)
						|NONE => (TextIO.output(TextIO.stdErr, "usage ec <filename> -o <outname> \n"); OS.Process.exit OS.Process.failure)
				)
			|[] 	=> (TextIO.output(TextIO.stdErr, "usage ec <filename> -o <outname> \n"); OS.Process.exit OS.Process.failure) 
	end	

val _ = readCommandLine (CommandLine.arguments())

(*val thisLexer = case CommandLine.arguments() of
		    []  => (TextIO.output(TextIO.stdErr, "No File provided\n"); OS.Process.exit OS.Process.failure)
		 |  [x] => makeFileLexer x
		 |  _   => (TextIO.output(TextIO.stdErr, "usage: ec file\n"); OS.Process.exit OS.Process.failure)*)

val thisLexer = makeFileLexer (!inFile)



fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

(* The portion of the code that does the actual compiling *)

val (program,_) = ExprParser.parse (0,thisLexer,print_error,())
val compiled_tree = Compiler.compile program
val executable  = Translate.translate compiled_tree
(*val _           = TextIO.output(TextIO.stdOut, Machine.programToString executable)*)
val content = "yeet"

fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end



val executable = "#include <stdio.h>\n"^
				 "#include <stdint.h>\n"^
				 "#define true 1\n"^
				 "#define false 0\n"^
					executable

val _ =if !compileStatus then writeFile (!outFile) (executable)  else print "\n COMPILATION ABORTED ERRORS FOUND \n"

end
