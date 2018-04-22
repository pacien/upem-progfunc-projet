(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

let reg_idx (Reg(idx, _)) = idx
let reg_val (Reg(_, value)) = value
let reg_compar l r = (reg_val l) - (reg_val r)

let regs_get reglist index =
  List.find (fun (Reg(idx, _)) -> idx = index) reglist |> reg_val

let regs_set reglist index value =
  Reg(index, value) :: List.filter (fun (Reg(idx, _)) -> idx != index) reglist

