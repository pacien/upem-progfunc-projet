(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

let instptr_mk urmcmd_list =
  let rec aux urmcmd_list count acc = match urmcmd_list with
    | [] -> acc
    | instr :: tail -> aux tail (count + 1) ((count, instr) :: acc)
  in InstPtr([], List.rev (aux urmcmd_list 0 []))

let instptr_move_up = function
  | InstPtr(instr :: list1, list2) -> InstPtr(list1, instr :: list2)
  | x -> x

let instptr_move_down = function
  | InstPtr(list1, instr :: list2) -> InstPtr(instr :: list1, list2)
  | x -> x

let instptr_get = function
  | InstPtr(_, x :: _) -> x
  | InstPtr(_, []) -> failwith "No instruction left"

let instptr_string instptr =
  let string_of_inst = function
    | URMZero(a) -> "URMZero " ^ (string_of_int a)
    | URMSucc(a) -> "URMSucc " ^ (string_of_int a)
    | URMCopy(a, b) -> "URMCopy " ^ (string_of_int a) ^ " " ^ (string_of_int b)
    | URMJump(a, b, c) -> "URMJump " ^ (string_of_int a) ^ " " ^ (string_of_int b) ^ " " ^ (string_of_int c)
  in let string_of_instptr (l, inst) = (string_of_int l) ^ ": " ^ string_of_inst inst
  in try string_of_instptr (instptr_get instptr) with _ -> "null"

let instptr_end = function
  | InstPtr(_, []) -> true
  | _ -> false

let rec instptr_jump ptr offset = match offset with
  | 0 -> ptr
  | _ when offset > 0 -> instptr_jump (instptr_move_up ptr) (offset - 1)
  | _ -> instptr_jump (instptr_move_down ptr) (offset + 1)
