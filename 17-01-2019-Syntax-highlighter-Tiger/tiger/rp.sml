

structure RP =
struct


fun load lexer = case lexer () of
		     SOME i => i :: load lexer
		   | NONE   => []





fun lexfile file = let val strm = TextIO.openIn file
		   in RPLex.makeLexer (fn n => TextIO.inputN(strm,n))
		   end




(* Running with a lexer *)
fun runWithLexer lexer = let fun loop ()= case lexer () of
						  	  NONE      => ()
					       |  SOME tok  => loop (Machine.read_token tok)
			 in loop ()
			 end


val _ =  ( case CommandLine.arguments() of
	       [] => (
	       			print("no file provided\n");
	       			OS.Process.exit OS.Process.failure
	       			)

	    |  xs => (List.map (runWithLexer o lexfile) xs; ())
	 )


end
