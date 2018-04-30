(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

(* Reads a file into a string. *)
val string_of_file : in_channel -> string

(* Converts lexemes into URM instructions. *)
val urm_program_of_lex : string list -> urmcmd list

(* Converts lexemes into EURM instructions. *)
val eurm_program_of_lex : string list -> eurmcmd list

(* Converts lexemes into registers. *)
val regs_of_lex : string list -> reg list

(* Parses the string representation of a program. *)
val program_of_string : (string list -> 'a list) -> string -> 'a list

(* Parses the string representation of serialized registers. *)
val regs_of_string : string -> reg list
