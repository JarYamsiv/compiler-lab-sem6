signature Expr_TOKENS =
sig
type ('a,'b) token
type svalue
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val EPS:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val IDENTIFIER: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Expr_LRVALS=
sig
structure Tokens : Expr_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
