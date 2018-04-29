(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

(* Strips out comments and rewrite/enumerate labels *)
val compile_preprocess : eurmcmd list -> eurmcmd list

(* Rewrites Dec, GEqPredicate, LEqPredicate, LTPredicate, Mult and ZeroPredicate *)
val compile_stage1 : eurmcmd list -> state -> eurmcmd list * state

(* Rewrites Add, GTPredicate and Sub *)
val compile_stage2 : eurmcmd list -> state -> eurmcmd list * state

(* Rewrites Goto *)
val compile_stage3 : eurmcmd list -> state -> eurmcmd list * state

(* Rewrites Inc, EqPredicate, Label and Zero *)
val compile_stage4 : eurmcmd list -> state -> urmcmd list * state

(* Transcompiles an EURM instruction sequence into URM *)
val urm_from_eurm : eurmcmd list -> urmcmd list

