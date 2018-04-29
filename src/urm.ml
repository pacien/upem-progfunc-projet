(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common
open Instptr
open Reg

(* Gives a new urm by moving down its instruction pointer *)
let urm_move_down urm = { instptr = instptr_move_down urm.instptr ; regs = urm.regs }

(* TODO: Verifier pour JUMP que a et b sont deux registres initialisés *)

(* Applies the current instruction pointed by the pointer of instruction. Modifies the pointer of instruction for every instruction *)
let urm_apply urm =
  let aux = function
    | _, URMZero(a) -> { instptr = urm.instptr ; regs = regs_set (urm.regs) a 0 } |> urm_move_down
    | _, URMCopy(a, b) when a != b -> { instptr = urm.instptr ; regs = regs_set urm.regs a (regs_get urm.regs b) } |> urm_move_down
    | _, URMCopy(a, b) -> failwith "Copy from one register to itself"
    | _, URMSucc(a) -> { instptr = urm.instptr ; regs = regs_set urm.regs a ((regs_get urm.regs a) + 1) } |> urm_move_down
    | _, URMJump(a, b, c) when (regs_get urm.regs a) = (regs_get urm.regs b) -> { instptr = (instptr_jump urm.instptr (fst (instptr_get urm.instptr) - c)) ; regs = urm.regs }
    | _, _ -> { instptr = urm.instptr ; regs = urm.regs } |> urm_move_down
  in if instptr_end urm.instptr then urm else aux (instptr_get urm.instptr)

let rec urm_run_pre pre = function
  | { instptr = InstPtr(_, []) ; regs = reg_list } -> reg_list
  | urm -> pre urm; urm_apply urm |> urm_run_pre pre

let urm_run = urm_run_pre (fun _ -> ())

let urm_run_trace =
  let print_func u =
    print_endline (instptr_string u.instptr);
    print_endline (regs_string u.regs);
    print_newline ()
  in urm_run_pre (print_func)

(* Creates an URM from a command list and a register list *)
let urm_mk cmd_list reg_list = { instptr = (instptr_mk cmd_list) ; regs = reg_list }
