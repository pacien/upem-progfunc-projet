(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common

let rec string_of_file f =
  try
    let str = input_line f
    in str ^ " " ^ (string_of_file f)
  with End_of_file -> ""

let rec program_of_lex = function
  | [] -> []
  | instr :: tail -> match (String.lowercase_ascii instr) :: tail with
    | "zero" :: arg_1 :: tail -> (URMZero (int_of_string arg_1)) :: (program_of_lex tail)
    | "succ" :: arg_1 :: tail -> (URMSucc (int_of_string arg_1)) :: (program_of_lex tail)
    | "copy" :: arg_1 :: arg_2 :: tail -> (URMCopy ((int_of_string arg_1), (int_of_string arg_2))) :: (program_of_lex tail)
    | "jump" :: arg_1 :: arg_2 :: arg_3 :: tail -> (URMJump ((int_of_string arg_1), (int_of_string arg_2), (int_of_string arg_3))) :: (program_of_lex tail)
    | _ -> raise Syntax_error

(* FIXME: reject multiple definition of a single register *)
let rec regs_of_lex = function
  | [] -> []
  | regnum :: regvalue :: tail -> Reg (int_of_string regnum, int_of_string regvalue) :: (regs_of_lex tail)
  | _ -> raise Syntax_error

let seq_from_string lexer_func str =  Str.split (Str.regexp "[\t\n(), ]+") str |> lexer_func
let program_of_string = seq_from_string program_of_lex
let regs_of_string = seq_from_string regs_of_lex
