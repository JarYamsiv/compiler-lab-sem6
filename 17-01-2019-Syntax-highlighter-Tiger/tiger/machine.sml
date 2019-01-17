(** * The reverse polish machine. *)

structure Machine =
struct

(*

The reverse polish machine is a machine with a stack of integers. It
supports the following operations.

1. Push an integer on to the stack

2. Execute a binary operator using the top two arguments of the stack
   and push the result on to the stack.

3. Clearing the stack

4. Printing the top and printing the entire stack contents.

We capture this "instruction set" as a ML data type (what else). You
can see this as the abstract syntax tree for the "assembly language"
for the reverse polish machine.

An "assembly language" program in rpn is just a list of such
instructions.

*)
datatype Colors
  = red
  | green
  | white
  | yellow
  | grey

datatype Inst = Print of string*Colors


type Program = Inst list



(*

You can now skip the rest of the module for the first reading and move
over to translate where the actual compiler exists. The rest of the
section is a simulator for the reverse polish machine.

*)

(** ** Simulator for the machine

The reverse polish machine consists of a stack. We also have an
exception that is raised where we encounter a stack underflow during
the operations of the machine.

*)

type Stack   = int list
exception StackUnderflow of Stack


fun prnt_red x= print("\u001b[31;1m"^x)
fun prnt_green x= print("\u001b[32;1m"^x)
fun prnt_white x= print("\u001b[37;1m"^x)
fun prnt_yellow x= print("\u001b[33;1m"^x)
fun prnt_grey x= print("\u001b[30;1m"^x)

(* This function performs a single instruction of the stack machine *)

fun  step (Print (x,c) )_              = (case c of
                                            red => (prnt_red x; [])
                                            | green => (prnt_green x; [])
                                            | white => (prnt_white x; [])
                                            | yellow => (prnt_yellow x; [])
                                            | grey   => (prnt_grey x; [])
                                        )


(* And finally this runs a program. *)




(* Conversion of machine instructions to strings *)




end
