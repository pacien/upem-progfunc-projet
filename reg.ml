(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

let reg_idx (Reg(idx, _)) = idx
let reg_val (Reg(_, value)) = value
let reg_compar reg1 reg2 = (reg_val reg1) - (reg_val reg2)

let regs_get reglist idx =
  List.find (fun (Reg(x,v)) -> x = idx) reglist |> reg_val

let regs_set reglist index value =
  Reg(index, value) :: List.filter (fun (Reg(x, v)) -> x != index) reglist
