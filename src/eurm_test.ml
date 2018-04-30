(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common
open Main
open Parser
open Urm
open Eurm
open Reg
open Kaputt

let run_eurm_prgm filename regs =
  let prgm = read_prgm (fun f -> eurm_program_of_lex f |> urm_from_eurm) filename
  in urm_mk prgm regs |> urm_run

let () =
  Test.add_simple_test
    ~title:"example_eurm_add"
    (fun () ->
       let output = run_eurm_prgm "../examples/add.eurm" [Reg(1, 5); Reg(2, 3)]
       in Assertion.equal (regs_get output 1) (5 + 3))

(*let () =
  Test.add_simple_test
    ~title:"example_eurm_factorial"
    (fun () ->
       let output = run_eurm_prgm "../examples/factorial.eurm" [Reg(1, 5)]
       in Assertion.equal (regs_get output 1) 120)
*)
let () =
  Test.add_simple_test
    ~title:"example_eurm_sum_first_integers"
    (fun () ->
       let output = run_eurm_prgm "../examples/sum-first-integers.eurm" [Reg(1, 51)]
       in Assertion.equal (regs_get output 1) (51*(51+1)/2))

let () =
  Test.add_simple_test
    ~title:"example_eurm_sum_first_odd_integers"
    (fun () ->
       let output = run_eurm_prgm "../examples/sum-first-odd-integers.eurm" [Reg(1, 51)]
       in Assertion.equal (regs_get output 1) 676)

let () = if Array.mem "run-tests" Sys.argv then Test.launch_tests ()
