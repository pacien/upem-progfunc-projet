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

(*Creates a pointer of instruction from an urm command list*)
let instptr_mk urmcmd_list =
	let rec aux urmcmd_list count acc  =
		match urmcmd_list with
		| [] -> acc
		| instruction::reste -> aux reste (count + 1) ((count,instruction)::acc)
	in InstPtr([],rev (aux urmcmd_list 0 []))

(*Moves the pointer to the previous instruction*)
let instptr_move_up = function
	| InstPtr([],list2)-> InstPtr([],list2)
	| InstPtr(instr::list1,list2) -> InstPtr(list1, instr::list2)

(*Moves the pointer to the next instruction*)
let instptr_move_down = function
	| InstPtr(list1,[])-> InstPtr(list1,[])
	| InstPtr(list1,instr::list2) -> InstPtr(instr::list1, list2)

(*Returns the couple from the current pointer position : (line,instruction) where instruction is an urm command or fails if there is no instruction pointed*)
let instptr_get = function
	| InstPtr(list1,(l,Zero(a))::tail)-> (l,Zero(a))
	| InstPtr(list1,(l,Succ(a))::tail) -> (l,Succ(a))
	| InstPtr(list1,(l,Copy(a,b))::tail) -> (l,Copy(a,b))
	| InstPtr(list1,(l,Jump(a,b,c))::tail) -> (l,Jump(a,b,c))
	| InstPtr(_,[])-> failwith "No instruction left"

(*Converts the current instruction pointed into a string (line and instruction formatted). If there is no instruction, returns "null"*)
let instptr_string instptr =
	let aux = function
		| l,Zero(a) -> (string_of_int l)^": Zero "^(string_of_int a)
		| l,Succ(a) -> (string_of_int l)^": Succ "^(string_of_int a)
		| l,Copy(a,b) -> (string_of_int l)^": Copy "^(string_of_int a)^" "^(string_of_int b)
		| l,Jump(a,b,c) -> (string_of_int l)^": Jump "^(string_of_int a)^" "^(string_of_int b)^" "^(string_of_int c)
	in try aux (instptr_get instptr) with
		| _ -> "null"

(*Returns true if the instruction pointer is not pointing on any instruction (end of the instruction list)*)
let instptr_end = function
	| InstPtr(_,[]) -> true
	| _ -> false

(*Returns the pointer of instruction after a jump decided by the given offset*)
let rec instptr_jump ptr offset =
if offset = 0 then ptr else
	if offset > 0 then instptr_jump (instptr_move_up ptr) (offset-1)
	else instptr_jump (instptr_move_down ptr) (offset+1)


let reg_idx (Reg(idx, _)) = idx

let reg_val (Reg(_, value)) = value

(*Compares two registers. Returns -1 if reg1 is lower than reg2, 1 if it is greater than reg2 or 0 if both are equals.*)
let reg_compar reg1 reg2 = (reg_val reg1) - (reg_val reg2)

(*Returns the value contained in the specified register in a register list*)
let regs_get reglist idx = List.find (fun (Reg(x,v)) -> x=idx) reglist |> reg_val

(*Set the value of the register to value, or creates it to the value specified if it does not exist*)
let regs_set reglist index value = Reg(index,value)::(List.filter (fun (Reg(x,v))-> x!=index) reglist)

(*Gives a new urm by moving down its instruction pointer*)
let urm_move_down urm = {instptr = (instptr_move_down urm.instptr); regs = urm.regs}


(*TODO: Verifier pour JUMP que a et b sont deux registres initialisés*)

(*Applies the current instruction pointed by the pointer of instruction. Modifies the pointer of instruction for every instruction*)
let urm_apply urm =
	let aux = function
		| _,Zero(a) -> {instptr = urm.instptr ; regs = regs_set (urm.regs) a 0} |> urm_move_down
		| _,Copy(a,b) when a!=b -> {instptr = urm.instptr ; regs = regs_set urm.regs a (regs_get urm.regs b)} |> urm_move_down
		| _,Copy(a,b) -> failwith "Copy from one register to itself"
		| _,Succ(a) -> {instptr = urm.instptr ; regs = regs_set urm.regs a ((regs_get urm.regs a)+1)} |> urm_move_down
		| _,Jump(a,b,c) when (regs_get urm.regs a)=(regs_get urm.regs b)-> {instptr = (instptr_jump urm.instptr (fst (instptr_get urm.instptr) - c)); regs = urm.regs}
		| _,_-> {instptr = urm.instptr; regs = urm.regs} |> urm_move_down
	in if instptr_end urm.instptr then urm else aux (instptr_get urm.instptr)

(*Launches the URM*)
let rec urm_run = function
	| {instptr = InstPtr(_,[]); regs = reg_list } -> reg_list
	| urm -> urm_apply urm |> urm_run

(*Creates an URM from a command list and a register list*)
let urm_mk cmd_list reg_list = {instptr = (instptr_mk cmd_list) ; regs = reg_list}
