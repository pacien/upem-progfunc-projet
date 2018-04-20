(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

(* Creates a pointer of instruction from an urm command list *)
let instptr_mk urmcmd_list =
  let rec aux urmcmd_list count acc =
    match urmcmd_list with
    | [] -> acc
    | instr :: tail -> aux tail (count + 1) ((count, instr) :: acc)
  in InstPtr([], List.rev (aux urmcmd_list 0 []))

(* Moves the pointer to the previous instruction *)
let instptr_move_up = function
  | InstPtr([], list2) -> InstPtr([], list2)
  | InstPtr(instr :: list1, list2) -> InstPtr(list1, instr :: list2)

(* Moves the pointer to the next instruction *)
let instptr_move_down = function
  | InstPtr(list1, []) -> InstPtr(list1, [])
  | InstPtr(list1, instr :: list2) -> InstPtr(instr :: list1, list2)

(* Returns the couple from the current pointer position : (line, instruction) where instruction is an urm command or fails if there is no instruction pointed *)
let instptr_get = function
  | InstPtr(list1, (l, Zero(a)) :: tail)-> (l, Zero(a))
  | InstPtr(list1, (l, Succ(a)) :: tail) -> (l, Succ(a))
  | InstPtr(list1, (l, Copy(a, b)) :: tail) -> (l, Copy(a, b))
  | InstPtr(list1, (l, Jump(a, b, c)) :: tail) -> (l, Jump(a, b, c))
  | InstPtr(_, [])-> failwith "No instruction left"

(* Converts the current instruction pointed into a string (line and instruction formatted). If there is no instruction, returns "null" *)
let instptr_string instptr =
  let aux = function
    | l, Zero(a) -> (string_of_int l) ^ ": Zero " ^ (string_of_int a)
    | l, Succ(a) -> (string_of_int l) ^ ": Succ " ^ (string_of_int a)
    | l, Copy(a, b) -> (string_of_int l) ^ ": Copy " ^ (string_of_int a) ^ " " ^ (string_of_int b)
    | l, Jump(a, b, c) -> (string_of_int l) ^ ": Jump " ^ (string_of_int a) ^ " " ^ (string_of_int b) ^ " " ^ (string_of_int c)
  in try aux (instptr_get instptr) with _ -> "null"

(* Returns true if the instruction pointer is not pointing on any instruction (end of the instruction list) *)
let instptr_end = function
  | InstPtr(_, []) -> true
  | _ -> false

let rec instptr_jump ptr offset = match offset with
  | 0 -> ptr
  | _ when offset > 0 -> instptr_jump (instptr_move_up ptr) (offset - 1)
  | _ -> instptr_jump (instptr_move_down ptr) (offset + 1)
