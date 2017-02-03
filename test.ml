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



 ]
;;


let () =
  run_test_tt_main suite
;;
