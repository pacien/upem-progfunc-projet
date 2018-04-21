(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

val compile_preprocess : eurmcmd list -> eurmcmd list
val compile_stage1 : eurmcmd list -> state -> eurmcmd list * state
val compile_stage2 : eurmcmd list -> state -> eurmcmd list * state
val compile_stage3 : eurmcmd list -> state -> eurmcmd list * state
val compile_stage4 : eurmcmd list -> state -> urmcmd list * state
val urm_from_eurm : eurmcmd list -> urmcmd list

