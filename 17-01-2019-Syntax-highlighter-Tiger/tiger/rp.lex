type lexresult             = Machine.Token option
fun eof ()                 = NONE


%%
%structure RPLex

whitespace=[\ \r\t\n];

d=[0-9]+;

keywords="array" | "if" | "then" | "else" | "while" | "for" | "to" | "do" | "let" | "in" | "end" | "of" | "break" | "nil" | "function" | "var" | "type" | "import" | "primitive" ;

%%

{whitespace}+     				=> (SOME (Machine.Print (yytext,Machine.white))       );
"/*".*"*/"          			=> (SOME (Machine.Print (yytext,Machine.grey))       );
["].*["]						=> (SOME (Machine.Print (yytext,Machine.yellow))       );
{keywords}			  			=> (SOME (Machine.Keyword (yytext))       );
[-+]?{d}([.]{d})?([eE][-+]?{d})?	=> (SOME (Machine.Numeric yytext)       );
[a-zA-Z_][a-zA-Z0-9_]*			=> (SOME (Machine.Identifier yytext)       );
[-(){};+=<>"':/*\,]				=> (SOME (Machine.Symbols yytext)       );

