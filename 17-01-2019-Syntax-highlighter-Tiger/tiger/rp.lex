type lexresult             = Machine.Inst option
fun eof ()                 = NONE
fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt  = toSigned o String.explode

%%
%structure RPLex
whitespace=[\ \r\t\n];
digit=[0-9];
%%
{whitespace}+     => (lex()  (* White spaces are ignored *) );
"#".*\n           => (lex()  (* A line comment *)           );
"array"			  => (SOME (Machine.Print (yytext,Machine.red))       );
"if"			  => (SOME (Machine.Print (yytext,Machine.red))       );

