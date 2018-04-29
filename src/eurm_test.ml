(*
 * UPEM / L3 / Functional programming / Project: URM
 * Pacien TRAN-GIRARD, Adam NAILI
 *)

open Common
open Urm
open Eurm
open Kaputt.Abbreviations

let () =
  Test.add_simple_test
    ~title:"example_eurm_factorial_conversion"
    (fun () ->
       let input_eurm = [
         Comment "Compute r1! and place the result in r1";
         ZeroPredicate (1, "r1=0");
         Goto "r1>0";
         Comment "r1 holds 0";
         Label "r1=0";
         Inc 1;
         Goto "done";
         Comment "r1 holds a positive integer";
         Label "r1>0";
         Copy (2, 1);
         Zero 1;
         Inc 1;
         Zero 3;
         Inc 3;
         Comment "main loop";
         Label "loop";
         Mult (1, 3);
         EqPredicate (2, 3, "done");
         Inc 3;
         Goto "loop";
         Label "done";
         Quit]
       and expected_urm = [
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
       in let output_urm = urm_from_eurm input_eurm
       in
         Assert.is_true (output_urm = expected_urm))

let () = if Array.mem "run-tests" Sys.argv then Test.launch_tests ()
