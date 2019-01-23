

structure Machine =
struct


datatype Colors
  = red
  | green
  | white
  | yellow
  | grey

datatype Token = Print of string*Colors
  | Keyword of string
  | Symbols of string
  | Identifier of string
  | Numeric of string
  | InvalidToken





(*helper functions to print string x in different colors*)

fun prnt_red x= print("\u001b[31;1m"^x^"\u001b[0m")
fun prnt_green x= print("\u001b[32;1m"^x^"\u001b[0m")
fun prnt_white x= print("\u001b[37;1m"^x^"\u001b[0m")
fun prnt_yellow x= print("\u001b[33;1m"^x^"\u001b[0m")
fun prnt_grey x= print("\u001b[30;1m"^x^"\u001b[0m")

(* This function reads a single token *)

fun  read_token (Print (x,c) )= (case c of
                           red => (prnt_red x)
                            | green => (prnt_green x)
                            | white => (prnt_white x )
                            | yellow => (prnt_yellow x )
                            | grey   => (prnt_grey x )
                            )
    | read_token (Keyword x)    =( prnt_red x)
    | read_token (Numeric x)    =( prnt_yellow x)
    | read_token (Identifier x) =( prnt_white x)
    | read_token (Symbols x)    =( prnt_green x)
    | read_token _              =( )






end
