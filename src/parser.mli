(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

(* Reads a file into a string. *)
val string_of_file : in_channel -> string

(* Converts lexemes into instructions. *)
val program_of_lex : string list -> urmcmd list

(* Converts lexemes into registers. *)
val regs_of_lex : string list -> reg list

(* Parses the string representation of a program. *)
val program_of_string : string -> urmcmd list

(* Parses the string representation of serialized registers. *)
val regs_of_string : string -> reg list
