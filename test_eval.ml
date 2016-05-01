open Ast
open Eval
open Parse
open Assertions

(* Check if a value is an instance of VError *)
let check_error v =
  match v with
  | VError _ -> true
  | _ -> false;;

(* Primitive types *)
TEST_UNIT = eval [] Unit                   === VUnit
TEST_UNIT = eval [] (Int 42)               === VInt 42
TEST_UNIT = eval [] (Bool true)            === VBool true
TEST_UNIT = eval [] (Bool false)           === VBool false
TEST_UNIT = eval [] (String "hello world") === VString "hello world"

(* binops *)

(* Int operators *)
TEST_UNIT = eval [] (parse_expr "3 + 5")  === VInt 8
TEST_UNIT = eval [] (parse_expr "3 - 5")  === VInt (-2)
TEST_UNIT = eval [] (parse_expr "3 * 5")  === VInt 15

(* Comparison: bools *)
TEST_UNIT = eval [] (parse_expr "false = true")  === VBool false
TEST_UNIT = eval [] (parse_expr "true = true")   === VBool true
TEST_UNIT = eval [] (parse_expr "false = false") === VBool true
TEST_UNIT = eval [] (parse_expr "true = false")  === VBool false

TEST_UNIT = eval [] (parse_expr "true > true")   === VBool false
TEST_UNIT = eval [] (parse_expr "true > false")  === VBool true
TEST_UNIT = eval [] (parse_expr "false > false") === VBool false
TEST_UNIT = eval [] (parse_expr "false > true")  === VBool false

TEST_UNIT = eval [] (parse_expr "true < true")   === VBool false
TEST_UNIT = eval [] (parse_expr "true < false")  === VBool false
TEST_UNIT = eval [] (parse_expr "false < false") === VBool false
TEST_UNIT = eval [] (parse_expr "false < true")  === VBool true

TEST_UNIT = eval [] (parse_expr "true >= true")   === VBool true
TEST_UNIT = eval [] (parse_expr "true >= false")  === VBool true
TEST_UNIT = eval [] (parse_expr "false >= false") === VBool true
TEST_UNIT = eval [] (parse_expr "false >= true")  === VBool false

TEST_UNIT = eval [] (parse_expr "true <= true")   === VBool true
TEST_UNIT = eval [] (parse_expr "true <= false")  === VBool false
TEST_UNIT = eval [] (parse_expr "false <= false") === VBool true
TEST_UNIT = eval [] (parse_expr "false <= true")  === VBool true

TEST_UNIT = eval [] (parse_expr "true <> true")   === VBool false
TEST_UNIT = eval [] (parse_expr "true <> false")  === VBool true
TEST_UNIT = eval [] (parse_expr "false <> false") === VBool false
TEST_UNIT = eval [] (parse_expr "false <> true")  === VBool true

(* Comparison: ints *)
TEST_UNIT = eval [] (parse_expr "3 = 3")  === VBool true
TEST_UNIT = eval [] (parse_expr "3 = 5")  === VBool false
TEST_UNIT = eval [] (parse_expr "3 = 1")  === VBool false

TEST_UNIT = eval [] (parse_expr "3 < 3")  === VBool false
TEST_UNIT = eval [] (parse_expr "3 < 5")  === VBool true
TEST_UNIT = eval [] (parse_expr "3 < 1")  === VBool false

TEST_UNIT = eval [] (parse_expr "3 > 3")  === VBool false
TEST_UNIT = eval [] (parse_expr "3 > 5")  === VBool false
TEST_UNIT = eval [] (parse_expr "3 > 1")  === VBool true

TEST_UNIT = eval [] (parse_expr "3 <= 3") === VBool true
TEST_UNIT = eval [] (parse_expr "3 <= 5") === VBool true
TEST_UNIT = eval [] (parse_expr "3 <= 1") === VBool false

TEST_UNIT = eval [] (parse_expr "3 >= 3") === VBool true
TEST_UNIT = eval [] (parse_expr "3 >= 5") === VBool false
TEST_UNIT = eval [] (parse_expr "3 >= 1") === VBool true

TEST_UNIT = eval [] (parse_expr "3 <> 3") === VBool false
TEST_UNIT = eval [] (parse_expr "3 <> 5") === VBool true
TEST_UNIT = eval [] (parse_expr "3 <> 1") === VBool true

(* Comparison: strings *)
TEST_UNIT = eval [] (BinOp (Eq, String "hij", String "hij"))    === VBool true
TEST_UNIT = eval [] (BinOp (Eq, String "hij", String "hji"))    === VBool false
TEST_UNIT = eval [] (BinOp (Eq, String "hij", String "hii"))    === VBool false

TEST_UNIT = eval [] (BinOp (Gt, String "hij", String "hij"))    === VBool false
TEST_UNIT = eval [] (BinOp (Gt, String "hij", String "hji"))    === VBool false
TEST_UNIT = eval [] (BinOp (Gt, String "hij", String "hii"))    === VBool true

TEST_UNIT = eval [] (BinOp (Lt, String "hij", String "hij"))    === VBool false
TEST_UNIT = eval [] (BinOp (Lt, String "hij", String "hji"))    === VBool true
TEST_UNIT = eval [] (BinOp (Lt, String "hij", String "hii"))    === VBool false

TEST_UNIT = eval [] (BinOp (GtEq, String "hij", String "hij"))  === VBool true
TEST_UNIT = eval [] (BinOp (GtEq, String "hij", String "hji"))  === VBool false
TEST_UNIT = eval [] (BinOp (GtEq, String "hij", String "hii"))  === VBool true

TEST_UNIT = eval [] (BinOp (LtEq, String "hij", String "hij"))  === VBool true
TEST_UNIT = eval [] (BinOp (LtEq, String "hij", String "hji"))  === VBool true
TEST_UNIT = eval [] (BinOp (LtEq, String "hij", String "hii"))  === VBool false

TEST_UNIT = eval [] (BinOp (NotEq, String "hij", String "hij")) === VBool false
TEST_UNIT = eval [] (BinOp (NotEq, String "hij", String "hji")) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq, String "hij", String "hii")) === VBool true

(* String concatenation *)
TEST_UNIT = eval [] (BinOp (Concat, String "hij", String "hji"))
              === VString "hijhji"

(* Invalid binops *)
TEST_UNIT = check_error (eval [] (parse_expr "() + true"))             === true
TEST_UNIT = check_error (eval [] (parse_expr "() * true"))             === true
TEST_UNIT = check_error (eval [] (parse_expr "() - true"))             === true
TEST_UNIT = check_error (eval [] (parse_expr "(3, 4) + (4, 3)"))       === true
TEST_UNIT = check_error (eval [] (parse_expr "(3, 4) * (4, 3)"))       === true
TEST_UNIT = check_error (eval [] (parse_expr "(3, 4) - (4, 3)"))       === true
TEST_UNIT = check_error (eval [] (parse_expr "(fun x -> x) + Left 3")) === true
TEST_UNIT = check_error (eval [] (parse_expr "(fun x -> x) * Left 3")) === true
TEST_UNIT = check_error (eval [] (parse_expr "(fun x -> x) - Left 3")) === true
TEST_UNIT = check_error (
  eval [] (BinOp (Plus, String "abc", String "def")))  === true
TEST_UNIT = check_error (
  eval [] (BinOp (Minus, String "abc", String "def"))) === true
TEST_UNIT = check_error (
  eval [] (BinOp (Times, String "abc", String "def"))) === true

TEST_UNIT = check_error (eval [] (parse_expr "() < true"))          === true
TEST_UNIT = check_error (eval [] (parse_expr "() > 3"))             === true
TEST_UNIT = check_error (eval [] (parse_expr "() <= (fun x -> x)")) === true
TEST_UNIT = check_error (eval [] (parse_expr "() >= Left 4"))       === true
TEST_UNIT = check_error (eval [] (parse_expr "() = false"))         === true
TEST_UNIT = check_error (eval [] (parse_expr "() <> (4, 5)"))       === true

TEST_UNIT = check_error (
  eval [] (BinOp (Lt, String "abc", Int 4)))                      === true
TEST_UNIT = check_error (
  eval [] (BinOp (Gt, String "abc", Bool false)))                 === true
TEST_UNIT = check_error (
  eval [] (BinOp (LtEq, String "abc", Unit)))                     === true
TEST_UNIT = check_error (
  eval [] (BinOp (GtEq, String "abc", Fun ("x", Var "x"))))       === true
TEST_UNIT = check_error (
  eval [] (BinOp (Eq, String "abc", Variant ("Left", Int 3))))    === true
TEST_UNIT = check_error (
  eval [] (BinOp (NotEq, String "abc", Pair (Int 3, Int 4))))     === true

TEST_UNIT = check_error (eval [] (parse_expr "() ^ true"))         === true
TEST_UNIT = check_error (eval [] (parse_expr "3 ^ (fun x -> x)"))  === true
TEST_UNIT = check_error (eval [] (parse_expr "(Left 3) ^ (2, 4)")) === true

(* Functional comparisons *)
TEST_UNIT = check_error (
  eval [] (parse_expr "(fun x -> x) = (fun y -> y)"))  === true
TEST_UNIT = check_error (
  eval [] (parse_expr "(fun x -> x) < (fun y -> y)"))  === true
TEST_UNIT = check_error (
  eval [] (parse_expr "(fun x -> x) > (fun y -> y)"))  === true
TEST_UNIT = check_error (
  eval [] (parse_expr "(fun x -> x) >= (fun y -> y)")) === true
TEST_UNIT = check_error (
  eval [] (parse_expr "(fun x -> x) <= (fun y -> y)")) === true
TEST_UNIT = check_error (
  eval [] (parse_expr "(fun x -> x) <> (fun y -> y)")) === true

(* If expressions *)
TEST_UNIT = eval [] (
  parse_expr "if true then true else false")  === VBool true
TEST_UNIT = eval [] (
  parse_expr "if false then true else false") === VBool false
TEST_UNIT = eval [] (
  parse_expr "if true then false else true")  === VBool false
TEST_UNIT = eval [] (
  parse_expr "if false then false else true") === VBool true

(* Unbound variable error *)
TEST_UNIT = check_error (eval [] (parse_expr "x")) === true

(* Simple let *)
TEST_UNIT = eval [] (parse_expr "let x = 3 in let y = 5 in x + y") === VInt 8

(* Simple letrec, 10! *)
let f10 = parse_expr "let rec fact = fun n ->
                      if n = 0 then 1 else n *fact (n - 1) in fact 10";;
TEST_UNIT = eval [] f10 === VInt 3628800

(* Letrec fails if recursive variable is evaluated on right side*)
TEST_UNIT = check_error (
  eval [] (parse_expr "let rec x = 3 + x in x")) === true

(* Simple app *)
TEST_UNIT = eval [] (parse_expr "(fun x -> 3 + x) 8") === VInt 11

(* App fails for invalid functions *)
TEST_UNIT = check_error (eval [] (parse_expr "(fun x -> x + true) 8")) === true

(* Simple fun *)
TEST_UNIT = eval [] (parse_expr "fun x -> 3 + x") === VClosure (
  "x", BinOp (Plus, Int 3, Var "x"), [])

(* Simple pair *)
TEST_UNIT = eval [] (parse_expr "(3 + 5, 4 < 3)") === VPair (
  VInt 8, VBool false)

(* Pair error: first elem *)
TEST_UNIT = check_error (eval [] (parse_expr "(3 + true, false)")) === true

(* Pair error: second elem *)
TEST_UNIT = check_error (eval [] (parse_expr "(3 + 4, () > true)")) === true

(* Simple variant *)
TEST_UNIT = eval [] (parse_expr "Left (3 + 5)") === VVariant ("Left", VInt 8)

(* Error in variant expression *)
TEST_UNIT = check_error (eval [] (parse_expr "Left (3 + true)")) === true

(* Simple match *)
TEST_UNIT = eval [] (
  parse_expr "match 3 with 1 -> true | 3 -> false")             === VBool false
TEST_UNIT = eval [] (
  parse_expr "match () with () -> true | true -> false")        === VBool true
TEST_UNIT = eval [] (
  parse_expr "match true with () -> true | true -> false")      === VBool false
TEST_UNIT = eval [] (
  parse_expr "match () with () -> true | true -> false")        === VBool true
TEST_UNIT = eval [] (
  parse_expr "match false with () -> true | x -> x")            === VBool false
TEST_UNIT = eval [] (
  parse_expr "match (3, 4) with (4, 3) -> () | (3, 4) -> true") === VBool true
TEST_UNIT = eval [] (
  parse_expr "match Left 3 with Right a -> a | Left b -> b")    === VInt 3
TEST_UNIT = eval [] (
  Match (String "a",
    [(PString "", Bool true); (PString "a", Bool false)]))      === VBool false

(* Match: error in match expression *)
TEST_UNIT = check_error (eval [] (
  parse_expr "match 3 + () with 1 -> true | 3 -> false")) === true

(* Match: pattern not found *)
TEST_UNIT = check_error (eval [] (
  parse_expr "match Left 3 with () -> 1 | 3 -> 1 | Right a -> a")) === true

(* Match: pattern not found in one element of pair *)
TEST_UNIT = check_error (eval [] (
  parse_expr "match (3, 4) with (x, 3) -> 1 | (4, y) -> 2")) === true

(* Match: duplicate binding in pattern *)
TEST_UNIT = check_error (eval [] (
  parse_expr "match (3, 4) with (x, x) -> x")) === true

(* Let: shadowing *)
TEST_UNIT = eval [] (
  parse_expr "let x = 3 in let y = 4 in
              let x = 5 in let y = x + y in y") === VInt 9

(* Let: unbound *)
TEST_UNIT = check_error (
  eval [] (parse_expr "let f = fun n -> f n in f 3")) === true

(* Deep match  on variants*)
TEST_UNIT = eval [] (parse_expr "
  match Cons (4, Cons (3, Nil ())) with
  | Cons (3, x) -> true
  | Cons (4, Cons (3, x)) -> false") === VBool false

(* Nested match *)
TEST_UNIT = eval [] (parse_expr "
  match Left Middle Right 3 with
  | Left x -> match x with
    | Left y -> false
    | Middle y -> match y with
      | Left z -> false
      | Middle z -> false
      | Right z -> true
    | Right y -> false
  | Middle x -> false
  | Right x -> false") === VBool true

(* Reverse a list *)
TEST_UNIT = eval [] (parse_expr "
  let rec rev =
    fun l ->
    fun acc ->
      match l with
      | Nil () -> acc
      | Cons (h, t) -> rev t Cons (h, acc) in
    rev (Cons (3, Cons (4, Cons (5, Nil ())))) (Nil ())") ===
  VVariant ("Cons", VPair (VInt 5,
  VVariant ("Cons", VPair (VInt 4,
  VVariant ("Cons", VPair (VInt 3, VVariant ("Nil", VUnit)))))))

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

TEST_UNIT = eval [] (App (App (gcdpos, Int 9), Int 33))        === VInt 3
TEST_UNIT = eval [] (App (App (gcdpos, Int 2), Int 2))         === VInt 2
TEST_UNIT = eval [] (App (App (gcdpos, Int 53248), Int 31636)) === VInt 4

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

TEST_UNIT = eval [] (App (catalan, Int 1))  === VInt 1
TEST_UNIT = eval [] (App (catalan, Int 3))  === VInt 5
TEST_UNIT = eval [] (App (catalan, Int 5))  === VInt 42
TEST_UNIT = eval [] (App (catalan, Int 10)) === VInt 16796

(* Dijkstra's algorithm. Graph is a list of pairs (v, n) where n is a list
of neighbors (v, w_v), v is a vertex (int), w_v are costs >= 0. Not efficient.*)
let shortest_distance = parse_expr "
  let rec remove =
    fun e ->
    fun f ->
      match e with
      | ((v_, d_), n) -> fun x -> if x = v_ then 0-1 else f x in
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
        fun visit ->
        fun minv ->
        fun gr ->
          match (gr, minv) with
          | (Nil (), vm) -> ((vm, visit vm), get_neighbors vm g)
          | (Cons ((v, n), t), vm) ->
            if vm = 0-1 then get_smallest visit v t
            else
              if visit vm = 0-1 then get_smallest visit v t
              else
                if visit v <> 0-1 then
                  if visit v < visit vm then get_smallest visit v t
                  else get_smallest visit vm t
                else get_smallest visit vm t in
      let rec helper =
        fun cur ->
        fun visit ->
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
            let visitn = remove cur visit in
            let rec get_cost =
              fun v ->
              fun nlist ->
                match nlist with
                | Nil () -> 0-1
                | Cons ((v_, w), t) -> if v = v_ then w else get_cost v t in
            let rec update =
              fun visit ->
              fun gr ->
                match gr with
                | Nil () -> visit
                | Cons ((v, n), t) ->
                  let c = get_cost v ncur in
                  if c = 0-1 then update visit t
                  else
                    if visit v = 0-1 then
                      update (fun x -> if x = v then c + dcur else visit x) t
                    else
                      if c + dcur >= visit v then update visit t
                      else
                        update (fun x -> if x = v then c + dcur else visit x) t
                  in
            let visitu = update visitn g in
            let s = get_smallest visitu (0-1) g in
            match s with
            | ((v, d), n) ->
              if d = 0-1 then 0-1 else helper s visitu in
      helper ((vi, 0), get_neighbors vi g) (fun x -> 0-1) in
  shortest_distance";;

let app_dijkstra vi vd exp =
  App (App (App (shortest_distance, Int vi), Int vd), exp);;

let g1 = parse_expr "Nil ()";;
let g2 = parse_expr "Cons ((0, Nil ()), Nil ())";;
let g3 = parse_expr "
  Cons ((0, Cons ((1, 3), Nil ())), Cons ((1, Nil ()), Nil ()))";;
let g4 = parse_expr "
  Cons (
    (1, Cons ((2, 3), Cons ((4, 20), Cons ((3, 7), Nil ())))), Cons (
    (2, Cons ((3, 5), Nil ())), Cons (
    (3, Cons ((4, 5), Nil ())), Cons (
    (4, Nil ()), Nil ()))))";;
let g5 = parse_expr "
  Cons (
    (1, Cons ((2, 20), Cons ((3, 10), Cons ((4, 3), Nil ())))), Cons (
    (2, Cons ((4, 3), Nil ())), Cons (
    (3, Cons ((5, 3), Cons ((2, 10), Cons ((4, 5), Nil ())))), Cons (
    (4, Cons ((5, 10), Cons ((3, 4), Nil ()))), Cons (
    (5, Cons ((2, 6), Nil ())), Nil ())
  ))))";;
let g6 = parse_expr "
  Cons ((1,
    Cons ((2, 100), Cons ((3, 50), Cons ((4, 5), Cons ((5, 3),
    Cons ((6, 75), Cons ((7, 90), Cons ((8, 95), Nil ())))))))),
  Cons ((2,
    Cons ((1, 10), Cons ((3, 12), Cons ((4, 14), Cons ((5, 3),
    Cons ((6, 16), Cons ((7, 18), Cons ((8, 20), Nil ())))))))),
  Cons ((3,
    Cons ((1, 20), Cons ((2, 45), Cons ((4, 3), Cons ((5, 10),
    Cons ((6, 20), Cons ((7, 35), Cons ((8, 50), Nil ())))))))),
  Cons ((4,
    Cons ((1, 3), Cons ((2, 90), Cons ((3, 45), Cons ((5, 20),
    Cons ((6, 70), Cons ((7, 100), Cons ((8, 200), Nil ())))))))),
  Cons ((5,
    Cons ((1, 10), Cons ((2, 90), Cons ((3, 41), Cons ((4, 21),
    Cons ((6, 80), Cons ((7, 83), Cons ((8, 90), Nil ())))))))),
  Cons ((6,
    Cons ((1, 5), Cons ((2, 30), Cons ((3, 20), Cons ((4, 10),
    Cons ((5, 10), Cons ((7, 10), Cons ((8, 16), Nil ())))))))),
  Cons ((7,
    Cons ((1, 3), Cons ((2, 10), Cons ((3, 30), Cons ((4, 11),
    Cons ((5, 12), Cons ((6, 13), Cons ((8, 5), Nil ())))))))),
  Cons ((8,
    Cons ((1, 1), Cons ((2, 3), Cons ((3, 11), Cons ((4, 5),
    Cons ((5, 2), Cons ((6, 1), Cons ((7, 2), Nil ())))))))),
  Nil ()))))))))";;

TEST_UNIT = eval [] (app_dijkstra 0 1 g1) === VInt (-1)
TEST_UNIT = eval [] (app_dijkstra 0 0 g2) === VInt 0
TEST_UNIT = eval [] (app_dijkstra 0 1 g2) === VInt (-1)
TEST_UNIT = eval [] (app_dijkstra 0 1 g3) === VInt 3
TEST_UNIT = eval [] (app_dijkstra 1 4 g4) === VInt 12
TEST_UNIT = eval [] (app_dijkstra 2 1 g4) === VInt (-1)
TEST_UNIT = eval [] (app_dijkstra 1 2 g5) === VInt 16
TEST_UNIT = eval [] (app_dijkstra 1 5 g5) === VInt 10
TEST_UNIT = eval [] (app_dijkstra 1 2 g6) === VInt 82

let () = Pa_ounit_lib.Runtime.summarize()