open Ast
open TypedAst

type equation = Eq of typ * typ

(******************************************************************************)
(** type substitution *********************************************************)
(******************************************************************************)

(**
 * These are useful functions for applying a substitution of a type for a type
 * variable
 *)

(** A substitution of a type for a type variable *)
type substitution = talpha * typ

(** apply a type substitution to a type *)
let rec subst_typ ((x,t'):substitution) (t:typ) =
  match t with
  | TAlpha y
      -> if y = x then t' else TAlpha y
  | TUnit | TInt | TBool | TString
      -> t
  | TArrow (t1,t2)
      -> TArrow (subst_typ (x,t') t1, subst_typ (x,t') t2)
  | TStar (t1,t2)
      -> TStar (subst_typ (x,t') t1, subst_typ (x,t') t2)
  | TVariant (ts, name)
      -> TVariant (List.map (subst_typ (x,t')) ts, name)

(** apply a type substitution to a list of equations *)
let subst_eqn (s : substitution) (eqns : equation list) : equation list =
  List.map (fun (Eq (t1,t2)) -> Eq(subst_typ s t1, subst_typ s t2)) eqns

(** apply a type substitution to an annotated expression
    we deliberately violate the 80-column restriction here to make the
    parallelism in the definition clearer, hence easier to read *)
let rec subst_expr (s : substitution) (e : annotated_expr) : annotated_expr =
  match e with
  | AVar      (t,x)            -> AVar      (subst_typ s t, x)
  | AApp      (t,e1,e2)        -> AApp      (subst_typ s t, subst_expr s e1, subst_expr s e2)
  | AFun      (t,(x,tx),e)     -> AFun      (subst_typ s t, (x, subst_typ s tx), subst_expr s e)
  | ALet      (t,(x,tx),e1,e2) -> ALet      (subst_typ s t, (x, subst_typ s tx), subst_expr s e1, subst_expr s e2)
  | ALetRec   (t,(x,tx),e1,e2) -> ALetRec   (subst_typ s t, (x, subst_typ s tx), subst_expr s e1, subst_expr s e2)
  | AUnit     (t)              -> AUnit     (subst_typ s t)
  | AInt      (t,n)            -> AInt      (subst_typ s t, n)
  | ABool     (t,b)            -> ABool     (subst_typ s t, b)
  | AString   (t,k)            -> AString   (subst_typ s t, k)
  | AVariant  (t,c,e)          -> AVariant  (subst_typ s t, c, subst_expr s e)
  | APair     (t,e1,e2)        -> APair     (subst_typ s t, subst_expr s e1, subst_expr s e2)
  | ABinOp    (t,op,e1,e2)     -> ABinOp    (subst_typ s t, op, subst_expr s e1, subst_expr s e2)
  | AIf       (t,e1,e2,e3)     -> AIf       (subst_typ s t, subst_expr s e1, subst_expr s e2, subst_expr s e3)
  | AMatch    (t,e,ps)         -> AMatch    (subst_typ s t, subst_expr s e, List.map (subst_case s) ps)
and subst_case s (p,e) = subst_pat s p, subst_expr s e
and subst_pat  s = function
  | APUnit    (t)              -> APUnit    (subst_typ s t)
  | APInt     (t,n)            -> APInt     (subst_typ s t, n)
  | APBool    (t,b)            -> APBool    (subst_typ s t, b)
  | APString  (t,k)            -> APString  (subst_typ s t, k)
  | APVar     (t,x)            -> APVar     (subst_typ s t, x)
  | APVariant (t,c,p)          -> APVariant (subst_typ s t, c, subst_pat s p)
  | APPair    (t,p1,p2)        -> APPair    (subst_typ s t, subst_pat s p1, subst_pat s p2)

(******************************************************************************)
(** helper functions **********************************************************)
(******************************************************************************)

(* you may find it helpful to implement these or other helper
 * functions, but they are not required.  Feel free to implement them if you
 * need them, change their types or arguments, delete them, whatever.
 *)

(** format a list of equations for printing *)
let rec format_eqns (f : Format.formatter) (eqns : equation list) : unit =
  match eqns with
  | Eq (t1, t2)::[] -> Format.fprintf f "[%a = %a]"
                       format_type t1 format_type t2
  | Eq (t1, t2)::t  -> Format.fprintf f "[%a = %a], %a"
                      format_type t1 format_type t2 format_eqns t
  | []              -> Format.fprintf f ""
and format_type (f : Format.formatter) (t : typ) : unit =
  match t with
  | TUnit           -> Format.fprintf f "unit"
  | TInt            -> Format.fprintf f "int"
  | TBool           -> Format.fprintf f "bool"
  | TString         -> Format.fprintf f "string"
  | TAlpha s        -> Format.fprintf f "'%s" s
  | TArrow (t1, t2) -> Format.fprintf f "%a -> %a"
                       format_type t1 format_type t2
  | TStar (t1, t2)  -> Format.fprintf f "%a * %a" format_type t1 format_type t2
  | TVariant (l, n) -> Format.fprintf f "(%a) %s" format_typelist l n
and format_typelist (f : Format.formatter) (l : typ list) : unit =
  match l with
  | h::[] -> Format.fprintf f "%a" format_type h
  | h::t  -> Format.fprintf f "%a, %a" format_type h format_typelist t
  | []    -> Format.fprintf f ""

(** use format_eqns to print a value to the console *)
let print_eqns     = Printer.make_printer format_eqns

(** use format_value to convert a value to a string *)
let string_of_eqns = Printer.make_string_of format_eqns

(** generate an unused type variable *)
let counter =
  let r = ref 0 in
  fun () -> let _ = r := 1 + !r in !r

let newvar () : typ =
  TAlpha (Format.sprintf "n%02i" (counter ()) )

(** return the constraints for a binary operator *)
let collect_binop (t : typ) (op : operator) (tl : typ) (tr : typ)
                     : equation list =
  match op with
  | Plus | Minus | Times -> [Eq (t, TInt); Eq (tl, TInt); Eq (tr, TInt)]
  | Gt | Lt | Eq | GtEq | LtEq | NotEq -> [Eq (t, TBool); Eq (tl, tr)]
  | Concat -> [Eq (t, TString); Eq (tl, TString); Eq (tr, TString)]

(** find the latest binding in the vars *)
let rec find (l : (var * typ) list) (x : var) : (var * typ) option =
  match l with
  | [] -> None
  | (v, t) :: tl -> if x = v then Some (v, t) else find tl x

(** construct a list of types from a list of variant names and a matching of
  * types to variable names. Also gives a dictionary associating variable names
  * to newvar names
  *)
let rec construct_type (l : talpha list) : typ list * (talpha -> talpha) =
  match l with
  | v::t ->
    begin
      let (vl, d) = construct_type t in
      let x = newvar () in
      match x with
      | TAlpha xs -> (x::vl, fun s -> if s = v then xs else d s)
      | _ -> failwith "newvar () generated something that wasnt a TAlpha"
    end
  | [] -> ([], fun s -> s)

(** convert all variable names to associated newvars in the dictionary *)
let rec convert_type (t : typ) (d : talpha -> talpha) : typ =
  match t with
  | TAlpha v -> TAlpha (d v)
  | TArrow (t1, t2) -> TArrow (convert_type t1 d, convert_type t2 d)
  | TStar (t1, t2) -> TStar (convert_type t1 d, convert_type t2 d)
  | TVariant (tl, tn) ->
    let rec convert_list l =
      match l with
      | h::t -> (convert_type h d)::(convert_list t)
      | [] -> []
    in TVariant (convert_list tl, tn)
  | _ -> t

(** get the type associated with the given constructor, and converts the
  * talpha names using the dictionary
  *)
let rec get_type (l : (tname * typ) list) (c : tname) (d : talpha -> talpha)
                    : typ =
  match l with
  | (cn, ty)::t -> if cn = c then convert_type ty d else get_type t c d
  | [] -> failwith ("unbound type constructor " ^ c)

(* get the relevant spec from a list of specs *)
let rec get_spec (l : variant_spec list) (c : constructor) : variant_spec =
  match l with
  | h::t ->
    let rec helper cl =
      match cl with
      | (ci, ti)::tc -> if ci = c then h else helper tc
      | [] -> get_spec t c in
    helper h.constructors
  | [] -> failwith ("unbound type constructor " ^ c)

(** return the constraints for an expr
  * vars refers to a data structure that stores the types of each of the
  * variables that have been defined.
  * It is a list of pairs (var, typ)
  *)
let rec collect_expr (specs : variant_spec list)
                     (vars : (var * typ) list)
                     (e : annotated_expr) : equation list =
      match e with
      | AVar (t1, v1) ->
        begin
          match find vars v1 with
          | None -> failwith ("Unbound variable " ^ v1)
          | Some (v, t) -> [Eq (t1, t)]
        end
      | AApp (t1, e1, e2) ->
        let dom = newvar () in
        let ran = newvar () in
        [Eq (t1, ran); Eq (dom, typeof e2); Eq (typeof e1, TArrow (dom, ran))] @
        (collect_expr specs vars e1) @
        (collect_expr specs vars e2)
      | AFun (t1, (v1, t2), e1) ->
        (Eq (t1, TArrow (t2, typeof e1)))::
          (collect_expr specs ((v1, t2)::vars) e1)
      | ALet (t1, (v1, t2), e1, e2) ->
        (Eq (t2, typeof e1))::(Eq (typeof e2, t1))::
        (collect_expr specs vars e1) @
        (collect_expr specs ((v1, t2)::vars) e2)
      | ALetRec (t1, (v1, t2), e1, e2) ->
        (Eq (t2, typeof e1))::(Eq (typeof e2, t1))::
        (collect_expr specs ((v1, t2)::vars) e1) @
        (collect_expr specs ((v1, t2)::vars) e2)
      | AUnit t1 -> [Eq (t1, TUnit)]
      | AInt (t1, i) -> [Eq (t1, TInt)]
      | ABool (t1, b) -> [Eq (t1, TBool)]
      | AString (t1, s) -> [Eq (t1, TString)]
      | APair (t1, e1, e2) ->
        (Eq (t1, TStar (typeof e1, typeof e2)))::
        (collect_expr specs vars e1) @
        (collect_expr specs vars e2)
      | ABinOp (t1, o, e1, e2) ->
        (collect_binop t1 o (typeof e1) (typeof e2)) @
        (collect_expr specs vars e1) @
        (collect_expr specs vars e2)
      | AIf (t1, e1, e2, e3) ->
        (Eq (typeof e1, TBool))::(Eq (t1, typeof e2))::(Eq (t1, typeof e3))::
        (collect_expr specs vars e1) @
        (collect_expr specs vars e2) @
        (collect_expr specs vars e3)
      | AMatch (t1, e1, l) ->
        begin
          match l with
          | [] -> (collect_expr specs vars e1)
          | (p1, ei)::t ->
            (Eq (t1, typeof ei))::(Eq (typeof e1, typeof_pat p1))::
            (collect_case specs vars p1 ei) @
            (collect_expr specs vars (AMatch (t1, e1, t)))
        end
      | AVariant (t1, c, e1) ->
        let vspec = get_spec specs c in
        let (vl, d) = construct_type vspec.vars in
        let ty = get_type vspec.constructors c d in
        (Eq (t1, TVariant (vl, vspec.name)))::
        (Eq (ty, typeof e1))::
        (collect_expr specs vars e1)

(** return the constraints for a match cases
  * tconst refers to the type of the parameters of the specific constructors
  * tvariant refers to the type of the variant as a whole
  *)
and collect_case specs vs (p : annotated_pattern) (e : annotated_expr)
                             : equation list =
  (collect_pat specs p) @ (collect_expr specs ((collect_vars p) @ vs) e)

(** return the constraints for a pattern *)
and collect_pat specs (p : annotated_pattern) : equation list =
  match p with
    | APUnit    (t1)         -> [Eq (t1, TUnit)]
    | APInt     (t1, n)      -> [Eq (t1, TInt)]
    | APBool    (t1, b)      -> [Eq (t1, TBool)]
    | APString  (t1, s)      -> [Eq (t1, TString)]
    | APVar     (t1, x)      -> []
    | APVariant (t1, c, p)   ->
      let vspec = get_spec specs c in
      let (vl, d) = construct_type vspec.vars in
      let ty = get_type vspec.constructors c d in
      (Eq (t1, TVariant (vl, vspec.name)))::
      (Eq (ty, typeof_pat p))::
      (collect_pat specs p)
    | APPair    (t1, p1, p2) ->
      [Eq (t1, TStar (typeof_pat p1, typeof_pat p2))] @
      (collect_pat specs p1)                          @
      (collect_pat specs p2)

(** return the variable bindings for a pattern *)
and collect_vars (p : annotated_pattern) : ((var * typ) list) =
  match p with
  | APVar     (t1, x)      -> [(x, t1)]
  | APPair    (t1, p1, p2) -> (collect_vars p1) @ (collect_vars p2)
  | APVariant (t1, c, p1)  -> collect_vars p1
  | _                      -> []

(******************************************************************************)
(** constraint generation                                                    **)
(******************************************************************************)

(**
 * collect traverses an expression e and returns a list of equations that must
 * be satisfied for e to typecheck.
 *)
let collect specs e =
  collect_expr specs [] e

(******************************************************************************)
(** constraint solver (unification)                                          **)
(******************************************************************************)

let rec occurs_in x = function
  | TAlpha y
      -> x = y
  | TArrow (t1,t2) | TStar (t1,t2)
      -> occurs_in x t1 || occurs_in x t2
  | TVariant (ts,_)
      -> List.exists (occurs_in x) ts
  | TUnit | TInt | TBool | TString
      -> false

(**
 * unify solves a system of equations and returns a list of
 * definitions for the type variables.
 *)
let rec unify eqns = match eqns with
  | [] -> []

  | Eq (t1,t2)::tl when t1 = t2
     -> unify tl

  | Eq ((TAlpha x as t1), (t as t2))::tl
  | Eq ((t as t1), (TAlpha x as t2))::tl
     -> if occurs_in x t
        then failwith (Format.asprintf "circular type constraint %a = %a"
                                       Printer.format_type t1
                                       Printer.format_type t2)
        else (x,t)::(unify (subst_eqn (x,t) tl))

  | Eq (TArrow (t1,t1'), TArrow (t2,t2'))::tl
  | Eq (TStar  (t1,t1'), TStar  (t2,t2'))::tl
     -> unify ((Eq (t1,t2))::(Eq (t1',t2'))::tl)

  | Eq ((TVariant (t1s, n1) as t1), (TVariant (t2s, n2) as t2))::tl
     -> if n1 <> n2
        then failwith (Format.asprintf "can't unify %a and %a"
                                       Printer.format_type t1
                                       Printer.format_type t2)
        else unify ((List.map2 (fun t1 t2 -> Eq (t1,t2)) t1s t2s)
                    @ tl)

  | Eq (t1,t2)::tl
     -> failwith (Format.asprintf "can't unify %a and %a"
                                  Printer.format_type t1
                                  Printer.format_type t2)

(******************************************************************************)
(** inference                                                                **)
(******************************************************************************)

(**
 * rename the type variables so that the first is "a", the
 * second "b", and so on.  Example:
 *
 *  rename_vars ('t23 -> 't17 -> 't23 -> int)
 *  is          ('a   -> 'b   -> 'a   -> int)
 *)
let rec simplify e =
  let rec alpha_of_int i =
    let let_of_int i = String.make 1 (char_of_int (i - 1 + int_of_char 'a')) in
    if i <= 0 then "" else (alpha_of_int (i/26))^(let_of_int (i mod 26))
  in

  let next_var  = ref 0 in

  let newvar () =
    next_var := 1 + !next_var;
    TAlpha (alpha_of_int !next_var)
  in

  let rec subst vars = function
    | TAlpha x -> if List.mem_assoc x vars then vars else (x,newvar())::vars
    | TUnit | TInt | TBool | TString -> vars
    | TArrow (t1,t2) | TStar (t1,t2) -> let vars' = subst vars t1 in
                                        subst vars' t2
    | TVariant (ts,_) -> List.fold_left subst vars ts
  in

  subst [] e

(**
 * given an expression, return the type for that expression,
 * failing if it cannot be typed.
 *)
let infer defs e =
  let annotated = annotate e in
  let eqns      = collect defs annotated in
  let solution  = unify eqns in
  let newtype   = List.fold_left (fun e s -> subst_expr s e) annotated solution in
  let simplify  = simplify (typeof newtype) in
  List.fold_right subst_expr simplify newtype

