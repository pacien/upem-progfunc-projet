(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

(* Returns the index of a register. *)
val reg_idx : reg -> regidx

(* Compares two register Ri and Rj.
 * It returns an integer less than, equal to, or greater than zero if
 * the first register index is respectively less than, equal to, or
 * greater than the second register index. *)
val reg_compar : reg -> reg -> int

(* Returns the register value of a register from its index. Fails if
 * there is not register with the sought register index. *)
val regs_get : reg list -> regidx -> regval

(* Set the value of the register to value,
 * or creates it to the value specified if it does not exist *)
val regs_set : reg list -> regidx -> regval -> reg list
