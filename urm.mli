(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

(* Runs an URM.
 * Returns all registers when the program halts. *)
val urm_run : urm -> reg list

(* Runs an URM in trace mode.
 * Returns all registers when the program halts. *)
val urm_run_trace : urm -> reg list

(* Makes an URM. *)
val urm_mk : urmcmd list -> reg list -> urm
