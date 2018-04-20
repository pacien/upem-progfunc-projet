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
  | "zero" :: arg_1 :: tail -> (Zero (int_of_string arg_1)) :: (program_of_lex tail)
  | "succ" :: arg_1 :: tail -> (Succ (int_of_string arg_1)) :: (program_of_lex tail)
  | "copy" :: arg_1 :: arg_2 :: tail -> (Copy ((int_of_string arg_1), (int_of_string arg_2))) :: (program_of_lex tail)
  | "jump" :: arg_1 :: arg_2 :: arg_3 :: tail -> (Jump ((int_of_string arg_1), (int_of_string arg_2), (int_of_string arg_3))) :: (program_of_lex tail)
  | _ -> raise Syntax_error

let program_of_string str =
  let lex = Str.split (Str.regexp "[\t\n(),]+") str
  in List.iter (fun s -> print_string s; print_newline ()) lex; program_of_lex lex
