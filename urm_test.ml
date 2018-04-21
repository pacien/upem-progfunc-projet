(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common
open Instptr
open Urm
open Kaputt.Abbreviations

let () =
  Test.add_simple_test
    ~title:"example_urm_add_program"
    (fun () ->
       let input_prgm = [
         Zero 0;
         Zero 3;
         Jump (1, 3, 6);
         Succ 0;
         Succ 3;
         Jump (3, 3, 2);
         Zero 3;
         Jump (2, 3, 11);
         Succ 0;
         Succ 3;
         Jump (3, 3, 7)]
       and input_regs = [
         Reg (1, 2);
         Reg (2, 3)]
       and expected_urm = {
         instptr = InstPtr ([], [
           (0, Zero 0);
           (1, Zero 3);
           (2, Jump (1, 3, 6));
           (3, Succ 0);
           (4, Succ 3);
           (5, Jump (3, 3, 2));
           (6, Zero 3);
           (7, Jump (2, 3, 11));
           (8, Succ 0);
           (9, Succ 3);
           (10, Jump (3, 3, 7))]);
         regs = [
           Reg (1, 2);
           Reg (2, 3)]}
       and expected_output = [
         Reg (0, 5);
         Reg (1, 2);
         Reg (2, 3);
         Reg (3, 3)]
       in let output_prgm = urm_mk input_prgm input_regs
       in let output_regs = urm_run output_prgm
       in
         Assert.is_true (output_prgm = expected_urm);
         Assert.is_true ((List.sort (fun (Reg(l, _)) (Reg(r, _)) -> compare l r) output_regs) = expected_output))

let () = if Array.mem "run-tests" Sys.argv then Test.launch_tests ()

