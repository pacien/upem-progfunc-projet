(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

let reg_idx (Reg(idx, _)) = idx
let reg_val (Reg(_, value)) = value
let reg_compar l r = (reg_val l) - (reg_val r)
let reg_string (Reg (index, value)) = "(" ^ (string_of_int index) ^ "," ^ (string_of_int value) ^ ")"

let regs_get reglist index = List.find (fun (Reg(idx, _)) -> idx = index) reglist |> reg_val
let regs_set reglist index value = Reg(index, value) :: List.filter (fun (Reg(idx, _)) -> idx != index) reglist
let regs_sort = List.sort (fun (Reg(l, _)) (Reg(r, _)) -> compare l r)
let regs_string reglist = regs_sort reglist |> List.map (reg_string) |> String.concat ","
