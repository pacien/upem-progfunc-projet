(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common
open Instptr
open Urm
open Kaputt.Abbreviations

let sort_regs = List.sort (fun (Reg(l, _)) (Reg(r, _)) -> compare l r)

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
         Assert.is_true ((sort_regs output_regs) = expected_output))

let () =
  Test.add_simple_test
    ~title:"example_urm_factorial_program"
    (fun () ->
       let input_prgm = [
         Zero 4;
         Jump (1, 4, 4);
         Zero 8;
         Jump (8, 8, 7);
         Succ 1;
         Zero 9;
         Jump (9, 9, 29);
         Copy (2, 1);
         Zero 1;
         Succ 1;
         Zero 3;
         Succ 3;
         Copy (5, 1);
         Zero 1;
         Zero 6;
         Jump (3, 6, 25);
         Zero 7;
         Jump (5, 7, 22);
         Succ 1;
         Succ 7;
         Zero 10;
         Jump (10, 10, 17);
         Succ 6;
         Zero 11;
         Jump (11, 11, 15);
         Jump (2, 3, 29);
         Succ 3;
         Zero 12;
         Jump (12, 12, 12);
         Zero 13;
         Jump (13, 13, 38)]
       and input_regs = [
         Reg (1, 5)]
       and expected_urm = {
         instptr = InstPtr ([], [
           (0, Zero 4);
           (1, Jump (1, 4, 4));
           (2, Zero 8);
           (3, Jump (8, 8, 7));
           (4, Succ 1);
           (5, Zero 9);
           (6, Jump (9, 9, 29));
           (7, Copy (2, 1));
           (8, Zero 1);
           (9, Succ 1);
           (10, Zero 3);
           (11, Succ 3);
           (12, Copy (5, 1));
           (13, Zero 1);
           (14, Zero 6);
           (15, Jump (3, 6, 25));
           (16, Zero 7);
           (17, Jump (5, 7, 22));
           (18, Succ 1);
           (19, Succ 7);
           (20, Zero 10);
           (21, Jump (10, 10, 17));
           (22, Succ 6);
           (23, Zero 11);
           (24, Jump (11, 11, 15));
           (25, Jump (2, 3, 29));
           (26, Succ 3);
           (27, Zero 12);
           (28, Jump (12, 12, 12));
           (29, Zero 13);
           (30, Jump (13, 13, 38))]);
         regs = [
           Reg (1, 5)]}
       and expected_output = [
         Reg (1, 120);
         Reg (2, 5);
         Reg (3, 5);
         Reg (4, 0);
         Reg (5, 24);
         Reg (6, 5);
         Reg (7, 24);
         Reg (8, 0);
         Reg (10, 0);
         Reg (11, 0);
         Reg (12, 0);
         Reg (13, 0)]
       in let output_prgm = urm_mk input_prgm input_regs
       in let output_regs = urm_run output_prgm
       in
         Assert.is_true (output_prgm = expected_urm);
         Assert.is_true ((sort_regs output_regs) = expected_output))

let () = if Array.mem "run-tests" Sys.argv then Test.launch_tests ()

