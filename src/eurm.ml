(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

let compile_preprocess eurmcmds = eurmcmds
let compile_stage1 eurmcmds state = eurmcmds, state
let compile_stage2 eurmcmds state = eurmcmds, state
let compile_stage3 eurmcmds state = eurmcmds, state
let compile_stage4 eurmcmds state = [URMZero(0)], state

let urm_from_eurm =
  let chain transform (eurmcmds, compile_state) = transform eurmcmds compile_state
  and initial_state = 0
  in (compile_preprocess, initial_state)
     |> chain compile_stage1
     |> chain compile_stage2
     |> chain compile_stage3
     |> chain compile_stage4

