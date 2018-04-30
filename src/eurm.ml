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

let build_initial_state eurmcmds =
  let max_reg_of_instr = function
    | Dec(r) | Inc(r) | Zero(r) | ZeroPredicate(r, _) -> r
    | Add(r1, r2) | Copy(r1, r2) | Mult(r1, r2) | Sub(r1, r2)
    | EqPredicate(r1, r2, _) | GEqPredicate(r1, r2, _) | GTPredicate(r1, r2, _)
    | LEqPredicate(r1, r2, _) | LTPredicate(r1, r2, _) -> max r1 r2
    | _ -> 0
  in {
    max_reg = List.fold_left (fun acc instr -> max acc (max_reg_of_instr instr)) 0 eurmcmds;
    label_count = List.fold_left (fun acc instr -> acc + (match instr with | Label(_) -> 1 | _ -> 0)) 0 eurmcmds
  }

let rec compile_stage1 eurmcmds state =
  let transform = function
    | Dec(r) ->
      let new_reg = state.max_reg + 1
      in [ Zero(new_reg); Inc(new_reg); Sub(r, new_reg) ],
         { max_reg = new_reg; label_count = state.label_count }

    | GEqPredicate(r1, r2, l) ->
      let new_reg = state.max_reg + 1
      in [ Copy(new_reg, r1); Inc(new_reg); GTPredicate(new_reg, r2, l) ],
         { max_reg = new_reg; label_count = state.label_count }

    | LEqPredicate(r1, r2, l) ->
      let new_reg = state.max_reg + 1
      in [ Copy(new_reg, r2); Inc(new_reg); GTPredicate(new_reg, r1, l) ],
         { max_reg = new_reg; label_count = state.label_count }

    | Mult(r1, r2) ->
      let ctr_reg = state.max_reg + 1 and res_reg = state.max_reg + 2
      and start_label = string_of_int (state.label_count + 1) and end_label = string_of_int (state.label_count + 2)
      in [ Zero(ctr_reg); Zero(res_reg); Label(start_label); EqPredicate(ctr_reg, r2, end_label);
           Add(res_reg, r1); Inc(ctr_reg); Goto(start_label); Label(end_label) ],
         { max_reg = state.max_reg + 2; label_count = state.label_count + 2}

    | ZeroPredicate(r, l) ->
      let new_reg = state.max_reg + 1
      in [ Zero(new_reg); EqPredicate(r, new_reg, l) ],
         { max_reg = new_reg; label_count = state.label_count }

    | LTPredicate(r1, r2, l) -> [ GTPredicate(r2, r1, l) ], state
    | any -> [ any ], state

  in match eurmcmds with
  | [] -> [], state
  | cmd :: tail ->
    let substitution, new_state = transform cmd
    in let prgm_tail, end_state = compile_stage1 tail new_state
    in substitution @ prgm_tail, end_state


let compile_stage2 eurmcmds state = eurmcmds, state
let compile_stage3 eurmcmds state = eurmcmds, state
let compile_stage4 eurmcmds state = [URMZero(0)], state

let urm_from_eurm eurmcmds =
  let chain transform (eurmcmds, compile_state) = transform eurmcmds compile_state
  in (compile_preprocess eurmcmds, build_initial_state eurmcmds)
     |> chain compile_stage1
     |> chain compile_stage2
     |> chain compile_stage3
     |> chain compile_stage4
     |> fst
