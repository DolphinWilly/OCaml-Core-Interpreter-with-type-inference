open Ast
open Eval
open Assertions

TEST_UNIT = eval [] (Int 42) === VInt 42

(* TODO: write more unit tests for [eval]

        _,  .-.  .-.
     ()(_ _ |  \/  |
         (_|||\__/|||
            ||    |||_
           _||   _||
          `""`  `""`
*)






let () = Pa_ounit_lib.Runtime.summarize()