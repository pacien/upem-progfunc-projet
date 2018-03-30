#load "str.cma";;
open List;;
(*line number*)
type line = int
(*register index*)
type regidx = int
(*register value*)
type regval = int
(*register*)
type reg = Reg of regidx * regval
(*URM instruction*)
type urmcmd =
|Copy of regidx * regidx
|Jump of regidx * regidx * line
|Succ of regidx
|Zero of regidx
(*instruction pointer*)
type instptr = InstPtr of (line * urmcmd) list * (line * urmcmd) list
(*URM*)
type urm = {instptr : instptr; regs : reg list}

exception Syntax_error

let rec string_of_file f =
	try
		let str = input_line f
	  in str ^ " " ^ (string_of_file f)
	with
	| End_of_file -> ""

let rec program_of_lex lex =
	match lex with
		|[] -> []
		|"zero" :: arg_1 :: tail ->(Zero (int_of_string arg_1)) :: (program_of_lex tail)
		|"succ" :: arg_1 :: tail -> (Succ (int_of_string arg_1)) :: (program_of_lex tail)
		|"copy" :: arg_1 :: arg_2 :: tail -> (Copy ((int_of_string arg_1), (int_of_string arg_2))):: (program_of_lex tail)
		|"jump" :: arg_1 :: arg_2 :: arg_3 :: tail ->(Jump ((int_of_string arg_1), (int_of_string arg_2),(int_of_string arg_3))):: (program_of_lex tail)
		|_ -> raise Syntax_error

let program_of_string str =
	let lex = Str.split (Str.regexp "[\t\n(),]+") str
	in List.iter (fun s -> print_string s; print_newline ()) lex; program_of_lex lex

let instptr_mk urmcmd_list =
	let rec aux urmcmd_list count acc  = 
		match urmcmd_list with
		| [] -> acc 
		| instruction::reste -> aux reste (count + 1) ((count,instruction)::acc) 
	in InstPtr([],rev (aux urmcmd_list 0 []))

let instptr_move_up = function
	| InstPtr(list1,[])-> InstPtr(list1,[])
	| InstPtr(list1,instr::list2) -> InstPtr(instr::list1, list2)

let instptr_get = function
	| _,Zero(_) -> Zero(_)
	| _,Succ(_) -> Succ(_)
	| _,Copy(_,_) -> Copy(_,_)
	| _,Jump(_,_,_) -> Jump(_,_,_)
	| _ -> raise Syntax_error