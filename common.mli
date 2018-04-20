(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

type line = int
type regidx = int
type regval = int
type reg = Reg of regidx * regval

type urmcmd =
  | Copy of regidx * regidx
  | Jump of regidx * regidx * line
  | Succ of regidx
  | Zero of regidx

type instptr = InstPtr of (line * urmcmd) list * (line * urmcmd) list

type urm = {
  instptr : instptr;
  regs : reg list
}

exception Syntax_error
