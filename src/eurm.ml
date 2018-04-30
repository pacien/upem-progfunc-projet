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

let add_reg_label state new_regs new_labels = {
  max_reg = state.max_reg + new_regs;
  label_count = state.label_count + new_labels
}

let rec apply_transform transform_func state = function
  | [] -> [], state
  | cmd :: tail ->
    let substitution, new_state = transform_func cmd
    in let prgm_tail, end_state = apply_transform transform_func new_state tail
    in substitution @ prgm_tail, end_state

let compile_stage1 eurmcmds state =
  let transform = function
    | Dec(r) ->
      let new_reg = state.max_reg + 1
      in [ Zero(new_reg); Inc(new_reg); Sub(r, new_reg) ],
         add_reg_label state 1 0

    | GEqPredicate(r1, r2, l) ->
      let new_reg = state.max_reg + 1
      in [ Copy(new_reg, r1); Inc(new_reg); GTPredicate(new_reg, r2, l) ],
         add_reg_label state 1 0

    | LEqPredicate(r1, r2, l) ->
      let new_reg = state.max_reg + 1
      in [ Copy(new_reg, r2); Inc(new_reg); GTPredicate(new_reg, r1, l) ],
         add_reg_label state 1 0

    | Mult(r1, r2) ->
      let ctr_reg = state.max_reg + 1 and res_reg = state.max_reg + 2
      and start_label = string_of_int (state.label_count + 1) and end_label = string_of_int (state.label_count + 2)
      in [ Zero(ctr_reg); Zero(res_reg); Label(start_label); EqPredicate(ctr_reg, r2, end_label);
           Add(res_reg, r1); Inc(ctr_reg); Goto(start_label); Label(end_label) ],
         add_reg_label state 2 2

    | ZeroPredicate(r, l) ->
      let new_reg = state.max_reg + 1
      in [ Zero(new_reg); EqPredicate(r, new_reg, l) ],
         add_reg_label state 1 0

    | LTPredicate(r1, r2, l) -> [ GTPredicate(r2, r1, l) ], state
    | any -> [ any ], state

  in apply_transform (transform) state eurmcmds

let compile_stage2 eurmcmds state =
  let transform = function
    | Add(r1, r2) ->
      let ctr_reg = state.max_reg + 1
      and start_label = string_of_int (state.label_count + 1) and end_label = string_of_int (state.label_count + 2)
      in [ Zero(ctr_reg); Label(start_label); EqPredicate(ctr_reg, r2, end_label);
           Inc(r1); Inc(ctr_reg); Goto(start_label); Label(end_label) ],
         add_reg_label state 1 2

    | GTPredicate(r1, r2, l) ->
      let aux_reg = state.max_reg + 1
      and start_label = string_of_int (state.label_count + 1) and end_label = string_of_int (state.label_count + 2)
      in [ Zero(aux_reg); Label(start_label); EqPredicate(aux_reg, r1, end_label); EqPredicate(aux_reg, r2, l);
           Inc(aux_reg); Goto(start_label); Label(end_label) ],
         add_reg_label state 1 2

    | Sub(r1, r2) ->
      let diff_reg = state.max_reg + 1 and aux1_reg = state.max_reg + 2 and aux2_reg = state.max_reg + 3
      and start_label = string_of_int (state.label_count + 1) and end_label = string_of_int (state.label_count + 2)
      and error_label = string_of_int (state.label_count + 3)
      in [ Zero(diff_reg); Copy(aux1_reg, r1); Copy(aux2_reg, r2); Label(start_label);
           EqPredicate(aux1_reg, r2, error_label); EqPredicate(aux2_reg, r1, end_label);
           Inc(diff_reg); Inc(aux1_reg); Inc(aux2_reg); Goto(start_label);
           Label(error_label); Quit; Label(end_label); Copy(r1, diff_reg) ],
         add_reg_label state 3 3

    | any -> [ any ], state

  in apply_transform (transform) state eurmcmds

let compile_stage3 eurmcmds state =
  let transform = function
    | Goto(lbl) ->
      let dummy_reg = state.max_reg + 1
      in [ Zero(dummy_reg); EqPredicate(dummy_reg, dummy_reg, lbl) ],
         add_reg_label state 1 0
    | any -> [ any ], state

  in apply_transform (transform) state eurmcmds

let compile_stage4 eurmcmds state =
  let label_table = Hashtbl.create 100
  in let build_label_table =
       List.iteri (fun lineo cmd -> match cmd with | Label(lbl) -> Hashtbl.add label_table lbl lineo | _ -> ())
  in let transform = function
    | Inc(r) -> [ URMSucc(r) ], state
    | Zero(r) -> [ URMZero(r) ], state
    | EqPredicate(r1, r2, lbl) -> [ URMJump(r1, r2, Hashtbl.find label_table lbl) ], state
    | Label(_) ->
      let dummy_reg = state.max_reg + 1
      in [ URMZero(dummy_reg) ], add_reg_label state 1 0
    | _ -> failwith "Invalid_argument"
  in build_label_table eurmcmds; apply_transform (transform) state eurmcmds

let urm_from_eurm eurmcmds =
  let chain transform (eurmcmds, compile_state) = transform eurmcmds compile_state
  in (compile_preprocess eurmcmds, build_initial_state eurmcmds)
     |> chain compile_stage1
     |> chain compile_stage2
     |> chain compile_stage3
     |> chain compile_stage4
     |> fst
