datatype keywords =ARRAY | IF | THEN | ELSE | WHILE | FOR | TO | DO | LET | IN | END | OF | BREAK | NIL | FUNCTION | VAR | TYPE | IMPORT | PRIMITIVE

datatype objectkeywords = CLASS
            | EXTENDS
            | METHOD
            | NEW
datatype symbols =  Comma | Colon | Semicolon | LeftB | RightB | LeftSB | RightSB | LeftCB | RightCB | Dot | Plus | Minus | Mul | Div | Equal | LTGT | LT | LTEqual | GT | GTEqual | AND | OR | ColonEqual
datatype whitespace = SPACE | TAB 

datatype Token = key of keywords
	|object of objectkeywords 
	| sym of symbols 	
	| white of whitespace*int 
	| NEWLINE 
	| EOF 
	| var of string 
	| comment of string
	| CONST of int
	| IDENTIFIER of string
	| QUOTE of string
	


