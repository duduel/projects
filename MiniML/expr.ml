(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let free_vars (exp : expr) : varidset =
  let rec free_vars' expr vars =
  match expr with
  | Var v -> SS.add v vars               
  | Num _ | Bool _ | Raise | Unassigned -> vars                         
  | Unop (_, x) -> free_vars' x vars                    
  | Binop (_, x, y) -> SS.union (free_vars' x vars)
                                  (free_vars' y vars)               
  | Conditional (x, y, z) -> SS.(union (free_vars' x vars)
                                         (free_vars' y vars)
                                 |> union (free_vars' z vars))             
  | Fun (v, x) -> SS.remove v (free_vars' x vars)          
  | Let (v, x, y) ->
      SS.(remove v
            (free_vars' y vars)
          |> union (free_vars' x vars))
  | Letrec (v, x, y) ->
      SS.(union (free_vars' x vars)
                  (free_vars' y vars)
          |> remove v)                   
  | App (x, y) -> SS.union (free_vars' x vars) (free_vars' y vars)
  
  in free_vars' exp (SS.empty) ;;
  
  
(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname =
    let ctr = ref 0 in
    fun () ->
    let indx = !ctr in
    ctr := !ctr + 1;
    "var" ^ string_of_int indx ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  if not (SS.mem var_name (free_vars exp)) then
    exp
  else
    let rec subst' (expr': expr) : expr =
        match expr' with
        | Var v -> (*Printf.printf "%s%s" v "yas";
        (match repl with
        | Num x ->  ignore x; *) if v = var_name then repl else expr'
        (*| _ -> expr') *)
        | Num _ | Bool _ | Raise | Unassigned -> expr'
        | Unop (un, x) -> Unop (un, subst' x)
        | Binop (bin, x, y) -> Binop (bin, subst' x, subst' y)
        | Conditional (x, y, z) -> Conditional (subst' x, subst' y, subst' z)
        | Fun (v, x) -> (*Printf.printf "%s%s%s%s" "[" v "]" "ysd";*)
            if SS.mem v (free_vars exp) then
              let fresh_var = new_varname () in
                Fun (fresh_var, subst' (subst v (Var fresh_var) x))
            else
              if v = var_name then expr'
              else Fun (v, subst' x)  
        | Let (v, x, y) ->
            if SS.mem v (free_vars exp) then
              let fresh_var = new_varname () in
                Let (fresh_var, subst' x, subst' (subst v (Var fresh_var) y))
            else
              Let (v, subst' x, if v = var_name then y else subst' y)
        | Letrec (v, x, y) ->
              if v = var_name then Letrec (v, x, y)
              else Letrec (v, subst' x, subst' y)
        | App (x, y) -> App (subst' x, subst' y)
    in subst' exp ;;

(*......................................................................
  String representations of expressions
 *)
   
    
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  let rec conc exp = exp_to_concrete_string exp
  in match exp with
  | Var v -> v
  | Num i -> string_of_int i
  | Bool b -> string_of_bool b
  | Unop (un, x) -> "-" ^ conc x
  | Binop (bin, x, y) ->
      (let binconc =
        match bin with
        | Plus -> " + "
        | Minus -> " - "
        | Times -> " * "
        | Equals -> " = "
        | LessThan -> " < "
      in conc x ^ binconc ^ conc y)
  | Conditional (x, y, z) ->
      "if " ^ conc x ^ " then " ^ conc y ^ " else " ^ conc z
  | Fun (v, x) -> "fun " ^ v ^ " -> " ^ conc x
  | Let (v, x, y) -> "let " ^ v ^ " = " ^ conc x ^ " in " ^ conc y
  | Letrec (v, x, y) -> "let rec " ^ v ^ " = " ^ conc x ^ " in " ^ conc y
  | Raise -> "raise"                        
  | Unassigned -> "unassigned"
  | App (x, y) -> "(" ^ conc x ^ ")" ^ conc y ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  let rec abstr exp = exp_to_abstract_string exp
  in match exp with
  | Var v -> "Var(" ^ v ^ ")"
  | Num i -> "Num(" ^ string_of_int i ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unop (un, x) -> "Unop(" ^ abstr x ^ ")"
  | Binop (bin, x, y) ->
      (let biabst =
        match bin with
        | Plus -> "Plus"
        | Minus -> "Minus"
        | Times -> "Times"
        | Equals -> "Equal"
        | LessThan -> "LessThan"
      in "Binop(" ^ biabst ^ ", " ^ abstr x ^ ", " ^ abstr y ^ ")")
  | Conditional (x, y, z) ->
      "Conditional(" ^ abstr x ^ ", " ^ abstr y ^ ", " ^ abstr z ^ ")"
  | Fun (v, x) -> "Fun(" ^ v ^ ", " ^ abstr x ^ ")"
  | Let (v, x, y) -> "Let(" ^ v ^ ", " ^ abstr x ^ ", " ^ abstr y ^ ")"
  | Letrec (v, x, y) -> "Letrec(" ^ v ^ ", " ^ abstr x ^ ", " ^ abstr y ^ ")"
  | Raise -> "Raise"                        
  | Unassigned -> "Unassigned"
  | App (x, y) -> "App(" ^ abstr x ^ ", " ^ abstr y ^ ")";;

  let test = subst "x" (Num 2) (Var ("x"))
             |> exp_to_concrete_string
  in Printf.printf "%s" test ;;

  let epr = App (Var("f"), App(Var("f"), Num 3));;

  let repr = "f";;
  let func = Fun ("x", Var ("x"));;

  let res = subst repr func epr

  let conc = exp_to_concrete_string res;;

  Printf.printf "\n%s\n" conc ;;
  