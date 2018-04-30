(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

type line = int
type label = string
type regidx = int
type regval = int
type reg = Reg of regidx * regval

type urmcmd =
  | URMCopy of regidx * regidx
  | URMJump of regidx * regidx * line
  | URMSucc of regidx
  | URMZero of regidx

type eurmcmd =
  | Add of regidx * regidx
  | Comment of string
  | Copy of regidx * regidx
  | Dec of regidx
  | EqPredicate of regidx * regidx * label
  | GEqPredicate of regidx * regidx * label
  | GTPredicate of regidx * regidx * label
  | Goto of label
  | Inc of regidx
  | Label of label
  | LEqPredicate of regidx * regidx * label
  | LTPredicate of regidx * regidx * label
  | Mult of regidx * regidx
  | Quit
  | Sub of regidx * regidx
  | Zero of regidx
  | ZeroPredicate of regidx * label

type instptr = InstPtr of (line * urmcmd) list * (line * urmcmd) list

type urm = {
  instptr : instptr;
  regs : reg list
}

type state = {
  max_reg : int;
  label_count : int;
  label_table : (string, int) Hashtbl.t
}

exception Syntax_error
