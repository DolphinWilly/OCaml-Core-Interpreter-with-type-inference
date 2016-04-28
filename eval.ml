open Ast

(******************************************************************************)
(** types (see .mli) **********************************************************)
(******************************************************************************)

type value =
  | VUnit | VInt of int | VBool of bool | VString of string
  | VClosure of var * expr * environment
  | VVariant of constructor * value
  | VPair of value * value
  | VError of string
and environment = (var * value ref) list

(******************************************************************************)
(** (optional) helper functions ***********************************************)
(******************************************************************************)

(** you may find it helpful to implement these or other helper
 * functions, but they are not required. Feel free to implement them if you
 * need them, change their types or arguments, delete them, whatever.
 *)

let rec string_of_value (v : value) : string =
  match v with
  | VUnit                -> "()"
  | VInt i               -> string_of_int i
  | VBool b              -> string_of_bool b
  | VString s            -> s
  | VClosure _           -> "<fun>"
  | VVariant (con, val_) -> con ^ " " ^ (string_of_value val_)
  | VPair (v1, v2)       -> "(" ^ (string_of_value v1) ^ ", "
                                ^ (string_of_value v2) ^ ")"
  | VError s             -> s

let string_of_binop (op : operator) : string =
  match op with
  | Plus   -> "+"
  | Minus  -> "-"
  | Times  -> "*"
  | Gt     -> ">"
  | Lt     -> "<"
  | Eq     -> "="
  | GtEq   -> ">="
  | LtEq   -> "<="
  | NotEq  -> "<>"
  | Concat -> "^"

 (**
  * try to match a value against a pattern. If the match succeeds, return an
  * environment containing all of the bindings. If it fails, return None.
  *)
let rec find_match (p : pattern) (v : value) : environment option =
  match (p, v) with
  | (PUnit, VUnit)           -> Some []
  | (PInt i1, VInt i2)       -> if i1 = i2 then Some [] else None
  | (PBool b1, VBool b2)     -> if b1 = b2 then Some [] else None
  | (PString s1, VString s2) -> if s1 = s2 then Some [] else None
  | (PVar x, val_)           -> Some [(x, ref val_)]
  | (PVariant (pcon, pat), VVariant (vcon, val_)) ->
    if pcon = vcon then find_match pat val_ else None
  | (PPair (pat1, pat2), VPair (val1, val2)) ->
    begin
      match (find_match pat1 val1, find_match pat2 val2) with
      | (Some env1, Some env2) -> Some (env1@env2)
      | _                      -> None
    end
  | _ -> None

(** apply the given operator to the given arguments *)
let rec eval_operator (op : operator) (v1 : value) (v2 : value) : value =
  match (op, v1, v2) with
  | (Plus, VInt a, VInt b)         -> VInt (a + b)
  | (Minus, VInt a, VInt b)        -> VInt (a - b)
  | (Times, VInt a, VInt b)        -> VInt (a * b)
  | (Gt, VInt a, VInt b)           -> VBool (a > b)
  | (Lt, VInt a, VInt b)           -> VBool (a < b)
  | (Eq, VInt a, VInt b)           -> VBool (a = b)
  | (GtEq, VInt a, VInt b)         -> VBool (a >= b)
  | (LtEq, VInt a, VInt b)         -> VBool (a <= b)
  | (NotEq, VInt a, VInt b)        -> VBool (a <> b)
  | (Gt, VString a, VString b)     -> VBool (a > b)
  | (Lt, VString a, VString b)     -> VBool (a < b)
  | (Eq, VString a, VString b)     -> VBool (a = b)
  | (GtEq, VString a, VString b)   -> VBool (a >= b)
  | (LtEq, VString a, VString b)   -> VBool (a <= b)
  | (NotEq, VString a, VString b)  -> VBool (a <> b)
  | (Gt, VBool a, VBool b)         -> VBool (a > b)
  | (Lt, VBool a, VBool b)         -> VBool (a < b)
  | (Eq, VBool a, VBool b)         -> VBool (a = b)
  | (GtEq, VBool a, VBool b)       -> VBool (a >= b)
  | (LtEq, VBool a, VBool b)       -> VBool (a <= b)
  | (NotEq, VBool a, VBool b)      -> VBool (a <> b)
  | (Concat, VString a, VString b) -> VString (a ^ b)
  | (_, VError s, _)               -> VError s
  | (_, _, VError s)               -> VError s
  | (o, x, y)                      -> VError ("Cannot apply binop '"
                                      ^ (string_of_binop o) ^ "' to '"
                                      ^ (string_of_value x) ^ "' and '"
                                      ^ (string_of_value y) ^ "'")

(* Lookup variable in the environment. Error if not found. *)
let rec lookup_var (env : environment) (x : var) : value =
  match env with
  | []           -> VError ("Unbound variable '" ^ x ^ "'")
  | (v, rval)::t -> if x = v then !rval else lookup_var t x

(** Format a value for printing. *)
let rec format_value (f : Format.formatter) (v : value) : unit =
  (* You will probably want to call Format.fprint f f <format string> <args>.
   *
   * Format.fprintf f <format string> has a different type depeding on the format
   * string. For example, Format.fprintf f "%s" has type string -> unit, while
   * Format.fprintf f "%i" has type int -> unit.
   *
   * Format.fprintf f "%a" is also useful. It has type
   *   (Format.formatter -> 'a -> unit) -> 'a -> unit
   * which is useful for recursively formatting values.
   *
   * Format strings can contain multiple flags and also other things to be
   * printed. For example (Format.fprintf f "result: %i %s") has type
   * int -> string -> unit, so you can write
   *
   *  Format.fprintf f "result: %i %s" 3 "blind mice"
   *
   * to output "result: 3 blind mice"
   *
   * See the documentation of the OCaml Printf module for the list of % flags,
   * and see the printer.ml for some (complicated) examples. Printer, format_type is
   * a nice example.
   *)
  failwith "unimplemented"

  (** use format_value to print a value to the console *)
let print_value = Printer.make_printer format_value

(*
(** use format_value to convert a value to a string *)
let string_of_value = Printer.make_string_of format_value
*)


(******************************************************************************)
(** eval **********************************************************************)
(******************************************************************************)

let rec eval env e =
  match e with
  | Unit               -> VUnit
  | Int i              -> VInt i
  | Bool b             -> VBool b
  | String s           -> VString s
  | BinOp (op, e1, e2) -> eval_operator op (eval env e1) (eval env e2)
  | If (cond, et, ef)  ->
    begin
      let vcond = eval env cond in
      match vcond with
      | VBool b -> if b then eval env et else eval env ef
      | VError s -> VError s
      | _ -> VError ("'" ^ (string_of_value vcond)
                         ^ "'is not a boolean condition")
    end
  | Var x              -> lookup_var env x
  | Let (x, e1, e2)    -> eval ((x, ref (eval env e1))::env) e2
  | LetRec (x, e1, e2) ->
    let vd = ref (VError ("'" ^ x
                              ^ "' cannot be evaluated on "
                              ^ "right-hand side of LetRec")) in
    let v1 = eval ((x, vd)::env) e1 in
    vd := v1;
    eval ((x, vd)::env) e2
  | App (e1, e2)       ->
    begin
      let v1 = eval env e1 in
      match v1 with
      | VClosure (x, exp, newenv) -> eval ((x, ref (eval env e2))::newenv) exp
      | VError s -> VError s
      | _ -> VError ("'" ^ (string_of_value v1) ^ "' cannot be applied; "
                         ^ "it is not a function")
    end
  | Fun (x, exp)       -> VClosure (x, exp, env)
  | Pair (e1, e2)      ->
    begin
      let (v1, v2) = (eval env e1, eval env e2) in
      match (v1, v2) with
      | (VError s, _) -> VError s
      | (_, VError s) -> VError s
      | _ -> VPair (v1, v2)
    end
  | Variant (con, exp) ->
    begin
      let v1 = eval env exp in
      match v1 with
      | VError s -> VError s
      | _ -> VVariant (con, eval env exp)
    end
  | Match (exp, lst)   ->
    begin
      let rec eval_match l =
        let v1 = eval env exp in
        match l with
        | [] -> VError ("Pattern match on '" ^ (string_of_value v1)
                                             ^ "' failed")
        | (pat, exp_)::t ->
          begin
            match find_match pat v1 with
            | Some newenv -> eval (newenv@env) exp_
            | None        -> eval_match t
          end in
      match (eval env exp) with
      | VError s -> VError s
      | _        -> eval_match lst
    end

