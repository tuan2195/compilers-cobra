open Compile
open Runner
open Printf
open OUnit2

let t name program expected = name>::test_run program name expected;;
let te name program expected = name>::test_err program name expected;;

let suite =
"suite">:::
 [
  t "forty" "let x = 40 in x" "40";
  t "fals" "let x = false in x" "false";
  t "tru" "let x = true in x" "true";
  t "true1" "if true : 5 else: 10" "5";
  t "false1" "if false : 5 else: 10" "10";
  t "print1" "print(5 - 10)" "-5\n-5";

  t "m1" "5 - 5" "0";
  t "m2" "5 + 5" "10";
  t "m3" "5 * 5" "25";
  t "m4" "5 - 0" "5";
  t "m5" "5 + 0" "5";
  t "m6" "5 * 0" "0";

  t "m7" "let x = 5 in x" "5";
  t "m8" "let x = 5, y = 6 in x + y" "11";
  t "m9" "let x = 5 + 6 in x" "11";
  t "m10" "let x = let y = 5 + 6 in y in x - 6" "5";
  t "m11" "let x = 5 in let y = 5 + x in y" "10";
  t "m12" "let x = 5, y = 6 in let z = x + y in z" "11";
  t "m13" "let x = 5, y = 6 in let z = let a = x + y in a in z" "11";

  t "m14" "let x = 5 in 5 * x" "25";
  t "m15" "let x = 5, y = 6 in x * y" "30";
  t "m16" "let x = 5, y = 6 in let z = let a = x * y in a in z" "30";
  (*t "false2" "if 5 : 5 else: 10" "10";*)
 ]
;;


let () =
  run_test_tt_main suite
;;
