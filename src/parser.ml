(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

let rec string_of_file f =
  try
    let str = input_line f
    in str ^ "\n" ^ (string_of_file f)
  with End_of_file -> ""

let rec eurm_program_of_lex = function
  | [] -> []
  | l -> match l with
    | "comment" :: comment :: tail -> Comment(comment) :: eurm_program_of_lex tail
    | "label" :: lbl :: tail -> Label(lbl) :: eurm_program_of_lex tail
    | "goto" :: lbl :: tail -> Goto(lbl) :: eurm_program_of_lex tail
    | "zero" :: r :: tail -> Zero(int_of_string r) :: eurm_program_of_lex tail
    | "inc" :: r :: tail -> Inc(int_of_string r) :: eurm_program_of_lex tail
    | "dec" :: r :: tail -> Dec(int_of_string r) :: eurm_program_of_lex tail
    | "copy" :: r1 :: r2 :: tail -> Copy(int_of_string r1, int_of_string r2) :: eurm_program_of_lex tail
    | "add" :: r1 :: r2 :: tail -> Add(int_of_string r1, int_of_string r2) :: eurm_program_of_lex tail
    | "sub" :: r1 :: r2 :: tail -> Sub(int_of_string r1, int_of_string r2) :: eurm_program_of_lex tail
    | "mult" :: r1 :: r2 :: tail -> Mult(int_of_string r1, int_of_string r2) :: eurm_program_of_lex tail
    | "eq?" :: r1 :: r2 :: lbl :: tail -> EqPredicate(int_of_string r1, int_of_string r2, lbl) :: eurm_program_of_lex tail
    | "geq?" :: r1 :: r2 :: lbl :: tail -> GEqPredicate(int_of_string r1, int_of_string r2, lbl) :: eurm_program_of_lex tail
    | "gt?" :: r1 :: r2 :: lbl :: tail -> GTPredicate(int_of_string r1, int_of_string r2, lbl) :: eurm_program_of_lex tail
    | "leq?" :: r1 :: r2 :: lbl :: tail -> LEqPredicate(int_of_string r1, int_of_string r2, lbl) :: eurm_program_of_lex tail
    | "lt?" :: r1 :: r2 :: lbl :: tail -> LTPredicate(int_of_string r1, int_of_string r2, lbl) :: eurm_program_of_lex tail
    | "zero?" :: r :: lbl :: tail -> ZeroPredicate(int_of_string r, lbl) :: eurm_program_of_lex tail
    | "quit" :: tail -> Quit :: eurm_program_of_lex tail
    | x -> String.concat " " x |> print_endline; raise Syntax_error

let rec urm_program_of_lex = function
  | [] -> []
  | l -> match l with
    | "zero" :: r :: tail -> URMZero(int_of_string r) :: urm_program_of_lex tail
    | "succ" :: r :: tail -> URMSucc(int_of_string r) :: urm_program_of_lex tail
    | "copy" :: r1 :: r2 :: tail -> URMCopy(int_of_string r1, int_of_string r2) :: urm_program_of_lex tail
    | "jump" :: r1 :: r2 :: l :: tail -> URMJump (int_of_string r1, int_of_string r2, int_of_string l) :: urm_program_of_lex tail
    | _ -> raise Syntax_error

(* TODO: reject multiple definition of a single register *)
let rec regs_of_lex = function
  | [] -> []
  | regnum :: regvalue :: tail -> Reg (int_of_string regnum, int_of_string regvalue) :: (regs_of_lex tail)
  | _ -> raise Syntax_error

let seq_from_string lexer_func str =
  String.lowercase_ascii str
  |> Str.global_replace (Str.regexp "comment.*\n") ""
  |> Str.split (Str.regexp "[\t\n(), ]+")
  |> lexer_func

let program_of_string lexer = seq_from_string lexer
let regs_of_string = seq_from_string regs_of_lex
