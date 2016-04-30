open Ast
open Eval
open Parse
open Infer
open TypedAst
open Assertions

let typeof_infer specs e =
  typeof (infer specs e);;

(* Assert that an exception is raised *)
let infer_raises specs e =
  try let _ = typeof (infer specs e) in failwith "error not raised" with
  | Failure "error not raised" ->
    assert ("exception should have" = "been raised")
  | _ -> ();;

(* Primitive types *)
TEST_UNIT = typeof_infer [] (parse_expr "()") === TUnit
TEST_UNIT = typeof_infer [] (parse_expr "42") === TInt
TEST_UNIT = typeof_infer [] (parse_expr "true") === TBool
TEST_UNIT = typeof_infer [] (String "hi") === TString

(* binops *)

(* Int operators *)
TEST_UNIT = typeof_infer [] (parse_expr "3 + 5")  === TInt

(* Comparison: bools *)
TEST_UNIT = typeof_infer [] (parse_expr "false = true")  === TBool

(* Comparison: ints *)
TEST_UNIT = typeof_infer [] (parse_expr "3 = 3") === TBool

(* Comparison: strings *)
TEST_UNIT = typeof_infer [] (BinOp (Eq, String "hij", String "hij")) === TBool

(* String concatenation *)
TEST_UNIT = typeof_infer [] (BinOp (Concat, String "hij", String "hji"))
    === TString

(* Let shadowing with different types *)
TEST_UNIT =  typeof_infer [] (parse_expr "let x = 3 in let x = true in x")  === TBool

(* If expressions *)
TEST_UNIT = typeof_infer [] (
  parse_expr "if true then 3+5 else 3-5")  === TInt

(* If expressions failure *)
TEST_UNIT = infer_raises [] (parse_expr "if true then true else ()") === ()

(* Unbound variable error *)
TEST_UNIT = infer_raises [] (parse_expr "x") === ()

(* Simple let *)
TEST_UNIT = typeof_infer [] (parse_expr "let x = 3 in let y = 5 in x + y") === TInt

(* Simple letrec, 10! *)
let f10 = parse_expr "let rec fact = fun n ->
                      if n = 0 then 1 else n *fact (n - 1) in fact";;
TEST_UNIT = typeof_infer [] f10 === TArrow (TInt, TInt)

(* Simple app *)
TEST_UNIT = typeof_infer [] (parse_expr "(fun x -> 3 + x) 8") === TInt

(* App fails for invalid functions *)
TEST_UNIT = infer_raises [] (parse_expr "(fun x -> x + true) 8") === ()

(* Simple fun *)
TEST_UNIT = typeof_infer [] (parse_expr "fun x -> 3 + x") === TArrow (TInt, TInt)

(* Simple pair *)
TEST_UNIT = typeof_infer [] (parse_expr "(3 + 5, 4 < 3)") === TStar (TInt, TBool)

(* Simple variant *)
let spec = parse_variant_spec "type ('a, 'b, 'c) t = Left of 'a | Middle of 'b | Right of 'c";;
TEST_UNIT = typeof_infer [spec] (parse_expr "Left (3 + 5)") === TVariant ([TInt; TAlpha "a"; TAlpha "b"], "t")

(* Simple match *)
TEST_UNIT = typeof_infer [] (
  parse_expr "match 3 with 1 -> true | 3 -> false") === TBool

(* Matching on variants *)
TEST_UNIT = typeof_infer [spec] (parse_expr "match Middle 3 with Left a -> a | Middle a -> a | Right a -> a") === TInt

(* Let: shadowing *)
TEST_UNIT = typeof_infer [] (
  parse_expr "let x = 3 in let y = 4 in
              let x = 5 in let y = x + y in y") === TInt

(* Let: unbound *)
TEST_UNIT = infer_raises [] (parse_expr "let f = fun n -> f n in f 3") === ()

(* Pair error: first elem *)
TEST_UNIT = infer_raises [] (parse_expr "(3 + true, false)") === ()

(* Pair error: second elem *)
TEST_UNIT = infer_raises [] (parse_expr "(3 + 4, () > true)") === ()

(* Invalid binop *)
TEST_UNIT = infer_raises [] (parse_expr "() + true") === ()

(* Match mismatch 1 *)
TEST_UNIT = infer_raises [] (parse_expr "match 3 with true -> 4 | 3 -> 3") === ()

(* Match mismatch 2 *)
TEST_UNIT = infer_raises [] (parse_expr "match 3 with 3 -> 4 | 4 -> true") === ()

(* Nested functions *)
TEST_UNIT = typeof_infer [] (parse_expr "let f = fun x -> x + 1 in let g = fun x -> x + 1 in let h = fun x -> x + 1 in f (g (h 1))") === TInt

(* Complicated function *)
TEST_UNIT = typeof_infer [] (parse_expr "fun x -> fun y -> fun z -> x z (y z)") ===
  parse_type "('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c"

(* From the PS *)
let list_spec = parse_variant_spec "type 'a list = Nil of unit | Cons of 'a * 'a list"

TEST_UNIT = typeof_infer [list_spec] (parse_expr "let x = Cons (1, Nil ()) in let y = Cons (true, Nil ()) in (x, y)") === parse_type "int list * bool list"

(* Multiple variants *)
let a_spec = parse_variant_spec "type ta = Right of unit";;
let b_spec = parse_variant_spec "type 'a tb = Middle of 'a";;
let c_spec = parse_variant_spec "type ('a, 'b) tc = Left of 'a * 'b";;

TEST_UNIT = typeof_infer [a_spec; b_spec; c_spec] (parse_expr "
  match ((Right (), Middle 3), Left ((4, 5), fun x -> fun y -> fun z -> ((x, y), z))) with
  | (x, Left (b, c)) ->
    match x with
    (Right (), Middle a) ->
      match b with
      | (d, e) -> (((a, d), e), c)")
  === parse_type "((int * int) * int) * ('a -> 'b -> 'c -> ('a * 'b) * 'c)"

(* Variants with functions *)
let f_spec = parse_variant_spec "type ('a, 'b) fa = Fun of 'a -> 'b";;

TEST_UNIT = typeof_infer [f_spec] (parse_expr "let f = fun x -> 3 in Fun f")
  === parse_type "('a, int) fa"

(* Deep match  on variants*)
TEST_UNIT = typeof_infer [list_spec] (parse_expr "
  match Cons (4, Cons (3, Nil ())) with
  | Cons (3, x) -> true
  | Cons (4, Cons (3, x)) -> false") === TBool

(* Nested match *)
let lmr_spec = parse_variant_spec "type 'a t = Left of 'a t | Middle of 'a t | Right of 'a";;
TEST_UNIT = typeof_infer [lmr_spec] (parse_expr "
  match Left (Middle (Right 3)) with
  | Left x -> match x with
    | Left y -> false
    | Middle y -> match y with
      | Left z -> false
      | Middle z -> false
      | Right z -> true
    | Right y -> false
  | Middle x -> false
  | Right x -> false") === TBool

(* Reverse a list *)
TEST_UNIT = typeof_infer [list_spec] (parse_expr "
  let rec rev =
    fun l ->
    fun acc ->
      match l with
      | Nil () -> acc
      | Cons (h, t) -> rev t Cons (h, acc) in
    rev (Cons (3, Cons (4, Cons (5, Nil ())))) (Nil ())") ===
  TVariant ([TInt], "list")

(* gcd on positive integers *)
let gcdpos = parse_expr "
  let rec mod =
    fun n ->
    fun m ->
      if n < m then n else mod (n - m) m in
  let rec gcd =
    fun m ->
    fun n ->
      if n = 0 then m else gcd n (mod m n) in
  gcd";;

TEST_UNIT = typeof_infer [] gcdpos === TArrow (TInt, TArrow (TInt, TInt))

(* count_bin_trees from ps1 *)
let catalan = parse_expr "
  let rec count_bin_trees =
    fun n ->
      if n = 0 then 1
      else
        let rec count_helper =
          fun m ->
          fun n ->
            count_bin_trees m * count_bin_trees n +
            (if n > 0 then count_helper (m + 1) (n - 1) else 0) in
        count_helper 0 (n - 1) in
  count_bin_trees";;

TEST_UNIT = typeof_infer [] catalan === TArrow (TInt, TInt)

(* Dijkstra's algorithm. Graph is a list of pairs (v, n) where n is a list
of neighbors (v, w_v), v is a vertex (int), w_v are costs >= 0. Not efficient.*)
let shortest_distance = parse_expr "
  let rec generate_lvisit =
    fun g ->
      match g with
      | Nil () -> Nil ()
      | Cons ((v, n), t) -> Cons ((v, 0-1), generate_lvisit t) in
  let rec remove =
    fun e ->
    fun l ->
      match l with
      | Nil () -> Nil ()
      | Cons ((v, d), t) ->
        match e with
        | ((v_, d_), n) ->
          if v = v_ then remove e t else Cons ((v, d), remove e t) in
  let rec get_neighbors =
    fun v ->
    fun g ->
      match g with
      | Nil () -> Nil ()
      | Cons ((v_, n), t) -> if v = v_ then n else get_neighbors v t in
  let rec shortest_distance =
    fun vi ->
    fun vd ->
    fun g ->
      let rec get_dist =
        fun av ->
          match av with
          | ((v, dist), n) -> dist in
      let rec get_smallest =
        fun lvisit ->
        fun minv ->
          match (lvisit, minv) with
          | (Nil (), (vm, dm)) -> ((vm, dm), get_neighbors vm g)
          | (Cons ((v, d), t), (vm, dm)) ->
            if dm = 0-1 then get_smallest t (v, d)
            else
              if d <> 0-1 then
                if d < dm then get_smallest t (v, d)
                else get_smallest t (vm, dm)
              else get_smallest t (vm, dm) in
      let rec helper =
        fun cur ->
        fun lvisit ->
          let vcur =
            match cur with
            | ((vc, dc), n) -> vc in
          if vcur = vd then get_dist cur
          else
            let dcur =
              match cur with
              | ((vc, dc), n) -> dc in
            let ncur =
              match cur with
              | (v, n) -> n in
            let lvisitn = remove cur lvisit in
            let rec get_cost =
              fun v ->
              fun nlist ->
                match nlist with
                | Nil () -> 0-1
                | Cons ((v_, w), t) -> if v = v_ then w else get_cost v t in
            let rec update =
              fun lvisit ->
                match lvisit with
                | Nil () -> Nil ()
                | Cons ((v, d), t) ->
                  let c = get_cost v ncur in
                  if c = 0-1 then Cons ((v, d), update t)
                  else
                    if d = 0-1 then Cons ((v, c + dcur), update t)
                    else
                      if c + dcur >= d then Cons ((v, d), update t)
                      else Cons ((v, c + dcur), update t) in
            let lvisitu = update lvisitn in
            match lvisitu with
            | Nil () -> 0-1
            | Cons (h, t) -> helper (get_smallest lvisitu (0, 0-1)) lvisitu in
      helper ((vi, 0), get_neighbors vi g) (generate_lvisit g) in
  shortest_distance";;

TEST_UNIT = typeof_infer [list_spec] shortest_distance ===
parse_type "int -> int -> (int * (int * int) list) list -> int"

let () = Pa_ounit_lib.Runtime.summarize()