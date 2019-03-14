val _ = print "BEGINING...\n"


signature TEMP = 
sig
	type temp
	type label
	val newlabel	:	unit -> label
	val newtemp 	: 	unit -> temp
end

signature EMPTY_SIG =  sig end

functor MakeTemp (structure A:EMPTY_SIG):TEMP = 
struct
	type temp = int
	type label = string

	val temp_count = ref 0
	val label_count = ref 0

	fun newlabel 	() = let val _ = label_count := (!label_count)+1 in "L" ^ (Int.toString (!label_count)) end
	fun newtemp 	() = let val _ = temp_count  := (!temp_count)+1  in (!temp_count)					    end
end

structure Temp = MakeTemp(structure A = struct end)

signature TREE = 
sig
	datatype exp = CONST of int
				 | NAME  of Temp.label
				 | TEMP  of Temp.temp
				 | BINOP of binop*exp*exp
				 | MEM of exp
				 | CALL of exp*exp list
				 | ESEQ of stm*exp

	and 	 stm = MOVE of exp*exp
			     | EXP of exp
			     | JUMP of exp*Temp.label list
			     | CJUMP of relop*exp*exp*Temp.label*Temp.label
			     | SEQ of stm*stm
			     | LABEL of Temp.label

	and		binop = PLUS | MINUS | MUL | DIV
				  | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

	and 	relop = EQ | NE | LT | GT | LE | GE
				  | ULT | ULE | UGT | UGE 	


end
