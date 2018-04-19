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
	| InstPtr([],list2)-> InstPtr([],list2)
	| InstPtr(instr::list1,list2) -> InstPtr(list1, instr::list2)

let instptr_move_down = function
	| InstPtr(list1,[])-> InstPtr(list1,[])
	| InstPtr(list1,instr::list2) -> InstPtr(instr::list1, list2)

let instptr_get = function
	| InstPtr(list1,(l,Zero(a))::tail)-> (l,Zero(a))
	| InstPtr(list1,(l,Succ(a))::tail) -> (l,Succ(a))
	| InstPtr(list1,(l,Copy(a,b))::tail) -> (l,Copy(a,b))
	| InstPtr(list1,(l,Jump(a,b,c))::tail) -> (l,Jump(a,b,c))
	| InstPtr(_,[])-> failwith "No instruction left"

let instptr_string instptr =
	let aux = function
		| l,Zero(a) -> (string_of_int l)^": Zero "^(string_of_int a)
		| l,Succ(a) -> (string_of_int l)^": Succ "^(string_of_int a)
		| l,Copy(a,b) -> (string_of_int l)^": Copy "^(string_of_int a)^" "^(string_of_int b)
		| l,Jump(a,b,c) -> (string_of_int l)^": Jump "^(string_of_int a)^" "^(string_of_int b)^" "^(string_of_int c)
	in try aux (instptr_get instptr) with
		| _ -> "null"


let reg_idx = function
	|	Reg(a,b) -> a

let reg_val = function
	|	Reg(a,b) -> b

let reg_compar reg1 reg2 = (reg_val reg1) - (reg_val reg2)

let regs_get reglist idx =
	let rec aux = function
		| [] -> failwith "Register not found"
		| Reg(i,v)::tail when i = idx -> v
		| Reg(_,_)::tail -> aux tail
	in aux reglist

let regs_exists regs idx = List.exists (fun (Reg(x,_)) -> x = idx) regs

(*TODO: Function of register manipulation: create a register or modify an existent register (remove + create?)*)


let urm_apply urm =
	let aux = function
		| _,Zero(a) -> urm.regs
	in aux (instptr_get urm.instptr)

let urm_run urm =
	match urm with
	| {instptr = InstPtr(_,[]); regs = reg_list } -> reg_list

let urm_mk cmd_list reg_list = {instptr = (instptr_mk cmd_list) ; regs = reg_list}
