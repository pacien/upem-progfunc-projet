(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common
open Instptr
open Urm
open Reg
open Kaputt.Abbreviations

let () =
  Test.add_simple_test
    ~title:"example_urm_add_program"
    (fun () ->
       let input_prgm = [
         URMZero 0;
         URMZero 3;
         URMJump (1, 3, 6);
         URMSucc 0;
         URMSucc 3;
         URMJump (3, 3, 2);
         URMZero 3;
         URMJump (2, 3, 11);
         URMSucc 0;
         URMSucc 3;
         URMJump (3, 3, 7)]
       and input_regs = [
         Reg (1, 2);
         Reg (2, 3)]
       and expected_urm = {
         instptr = InstPtr ([], [
           (0, URMZero 0);
           (1, URMZero 3);
           (2, URMJump (1, 3, 6));
           (3, URMSucc 0);
           (4, URMSucc 3);
           (5, URMJump (3, 3, 2));
           (6, URMZero 3);
           (7, URMJump (2, 3, 11));
           (8, URMSucc 0);
           (9, URMSucc 3);
           (10, URMJump (3, 3, 7))]);
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
         Assert.is_true ((regs_sort output_regs) = expected_output))

let () =
  Test.add_simple_test
    ~title:"example_urm_factorial_program"
    (fun () ->
       let input_prgm = [
         URMZero 4;
         URMJump (1, 4, 4);
         URMZero 8;
         URMJump (8, 8, 7);
         URMSucc 1;
         URMZero 9;
         URMJump (9, 9, 29);
         URMCopy (2, 1);
         URMZero 1;
         URMSucc 1;
         URMZero 3;
         URMSucc 3;
         URMCopy (5, 1);
         URMZero 1;
         URMZero 6;
         URMJump (3, 6, 25);
         URMZero 7;
         URMJump (5, 7, 22);
         URMSucc 1;
         URMSucc 7;
         URMZero 10;
         URMJump (10, 10, 17);
         URMSucc 6;
         URMZero 11;
         URMJump (11, 11, 15);
         URMJump (2, 3, 29);
         URMSucc 3;
         URMZero 12;
         URMJump (12, 12, 12);
         URMZero 13;
         URMJump (13, 13, 38)]
       and input_regs = [
         Reg (1, 5)]
       and expected_urm = {
         instptr = InstPtr ([], [
           (0, URMZero 4);
           (1, URMJump (1, 4, 4));
           (2, URMZero 8);
           (3, URMJump (8, 8, 7));
           (4, URMSucc 1);
           (5, URMZero 9);
           (6, URMJump (9, 9, 29));
           (7, URMCopy (2, 1));
           (8, URMZero 1);
           (9, URMSucc 1);
           (10, URMZero 3);
           (11, URMSucc 3);
           (12, URMCopy (5, 1));
           (13, URMZero 1);
           (14, URMZero 6);
           (15, URMJump (3, 6, 25));
           (16, URMZero 7);
           (17, URMJump (5, 7, 22));
           (18, URMSucc 1);
           (19, URMSucc 7);
           (20, URMZero 10);
           (21, URMJump (10, 10, 17));
           (22, URMSucc 6);
           (23, URMZero 11);
           (24, URMJump (11, 11, 15));
           (25, URMJump (2, 3, 29));
           (26, URMSucc 3);
           (27, URMZero 12);
           (28, URMJump (12, 12, 12));
           (29, URMZero 13);
           (30, URMJump (13, 13, 38))]);
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
         Assert.is_true ((regs_sort output_regs) = expected_output))

let () = if Array.mem "run-tests" Sys.argv then Test.launch_tests ()

