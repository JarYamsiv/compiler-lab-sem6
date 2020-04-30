structure tigh = 
 struct
val interactive = TIGHlex.makeLexer (fn _ => TextIO.inputN (TextIO.stdIn,1))

fun lexfile file = let val strm = TextIO.openIn file
		   in TIGHlex.makeLexer (fn n => TextIO.inputN(strm,n))
		   end

(*

TODO: For standard input which is not from the terminal, we would like
to use a more efficient lexer than interactive. This will be relevent
when we want to pipe the output to rp.

*)

(* Running with a lexer *)
fun runWithLexer lexer = let fun loop () = case lexer () of
						  EOF      => print("\027[0m\n")
					       |   inst => loop (myprint inst)
			 in loop ()
			 end


val _ =  ( case CommandLine.arguments() of
	       [] => runWithLexer interactive
	    |  xs => (List.map (runWithLexer o lexfile) xs; ())
	 )
	
end

