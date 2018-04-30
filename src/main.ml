(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common
open Parser
open Instptr
open Reg
open Urm
open Eurm

let exec_with_resource func filename =
  let file = open_in filename in
  let res = func file in
  close_in file; res

let read_prgm lexer = exec_with_resource (fun f -> string_of_file f |> program_of_string lexer)
let read_regs = exec_with_resource (fun f -> string_of_file f |> regs_of_string)
let run run_func prgm regs = urm_mk prgm regs |> run_func |> regs_string |> print_endline

let run_mode_of_string = function
  | "run" -> urm_run
  | "trace" -> urm_run_trace
  | _ -> failwith "Invalid run mode"

let lexer_of_string = function
  | "urm" -> (urm_program_of_lex)
  | "eurm" -> (fun toks -> eurm_program_of_lex toks |> urm_from_eurm)
  | _ -> failwith "Invalid lang"

let () = match Sys.argv with
  | [| _; "run-tests" |] -> () (* handled in test files *)
  | [| _; mode; lang; prgm; regs |] -> run (run_mode_of_string mode) (read_prgm (lexer_of_string lang) prgm) (read_regs regs)
  | _ -> print_endline "Usage: urm <run | trace> <urm | eurm> <prgmfile> <regfile>"
