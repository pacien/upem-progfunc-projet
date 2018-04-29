(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

let compile_preprocess =
  let rec label_table = Hashtbl.create 100
  and id_from_name name = match Hashtbl.find_opt label_table name with
    | Some(id) -> id
    | None -> let new_id = string_of_int (Hashtbl.length label_table)
              in Hashtbl.add label_table name new_id; new_id
  and aux = function
    | [] -> []
    | Comment(_) :: tail -> aux tail
    | Label(name) :: tail -> Label(id_from_name name) :: aux tail
    | EqPredicate(i, j, name) :: tail -> EqPredicate(i, j, id_from_name name) :: aux tail
    | GEqPredicate(i, j, name) :: tail -> GEqPredicate(i, j, id_from_name name) :: aux tail
    | GTPredicate(i, j, name) :: tail -> GTPredicate(i, j, id_from_name name) :: aux tail
    | LEqPredicate(i, j, name) :: tail -> LEqPredicate(i, j, id_from_name name) :: aux tail
    | LTPredicate(i, j, name) :: tail -> LTPredicate(i, j, id_from_name name) :: aux tail
    | ZeroPredicate(i, name) :: tail -> ZeroPredicate(i, id_from_name name) :: aux tail
    | Goto(name) :: tail -> Goto(id_from_name name) :: aux tail
    | any :: tail -> any :: aux tail
  in aux

let compile_stage1 eurmcmds state = eurmcmds, state
let compile_stage2 eurmcmds state = eurmcmds, state
let compile_stage3 eurmcmds state = eurmcmds, state
let compile_stage4 eurmcmds state = [URMZero(0)], state

let urm_from_eurm eurmcmds =
  let chain transform (eurmcmds, compile_state) = transform eurmcmds compile_state
  and initial_state = 0
  in (compile_preprocess eurmcmds, initial_state)
     |> chain compile_stage1
     |> chain compile_stage2
     |> chain compile_stage3
     |> chain compile_stage4
     |> fst
