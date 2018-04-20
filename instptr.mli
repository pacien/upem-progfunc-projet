(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

(* Create an instruction pointer for an URM program. *)
val instptr_mk : urmcmd list -> instptr

(* Move the instruction pointer up. Do nothing if this is not possible. *)
val instptr_move_up : instptr -> instptr

(* Move the instruction pointer down. Do nothing if this is not possible. *)
val instptr_move_down : instptr -> instptr

(* Get the current command from the instruction pointer.
 * Fail if the command pointer is not set on a valid command. *)
val instptr_get : instptr -> line * urmcmd

(* Get the current instruction as a string.
 * Returns "null" is the instruction pointer is not valid. *)
val instptr_string : instptr -> string

(* Returns the pointer of instruction after a jump decided by the given offse t *)
val instptr_jump : instptr -> int -> instptr

(* Returns true if the instruction pointer is not pointing on any instruction (end of the instruction list) *)
val instptr_end : instptr -> bool
