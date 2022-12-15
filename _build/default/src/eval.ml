
open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
  Value v -> v
  |ID(i) -> lookup env i
  |Not(ex) -> if eval_expr env ex = Bool true then Bool false
  else if eval_expr env ex = Bool false then Bool true
  else raise (TypeError "expected bool")
  |Binop(op, e1, e2) -> (match op with
    Add ->  (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (Int i, Int j) -> Int(i + j)
      |(_,_) -> raise (TypeError "expected int"))
    |Sub -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (Int i, Int j) -> Int(i - j)
      |_ -> raise (TypeError "expected int"))
    |Mult -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (Int i, Int j) -> Int(i * j)
      |_ -> raise (TypeError "expected int"))
    |Div -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (Int i, Int j) -> if j = 0 then raise(DivByZeroError) else Int(i/j)
      |_ -> raise (TypeError "expected int"))
    |Greater -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (Int i, Int j) -> Bool (i > j)
      |_ -> raise (TypeError "expected int"))
    |Less -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (Int i, Int j) -> Bool (i < j)
      |_ -> raise (TypeError "expected int"))
    |GreaterEqual -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (Int i, Int j) -> Bool (i >= j)
      |_ -> raise (TypeError "expected int"))
    |LessEqual -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (Int i, Int j) -> Bool (i <= j)
      |_ -> raise (TypeError "expected int"))
    |Concat -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (String i, String j) -> String (i^j)
      |_ -> raise (TypeError "expected string"))
    |Equal -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (String i, String j) -> Bool (i = j)
      |(Int i, Int j) -> Bool (i = j)
      |(Bool i, Bool j) -> Bool (i = j)
      |_ -> raise (TypeError "not same type"))
    |NotEqual -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (String i, String j) -> Bool (not (i = j))
      |(Int i, Int j) -> Bool (not (i = j))
      |(Bool i, Bool j) -> Bool (not (i = j))
      |_ -> raise (TypeError "not same type"))
    |Or -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (Bool i, Bool j) -> Bool (i || j)
      |_ -> raise (TypeError "expected bool"))
    |And -> (let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
    match (v1, v2) with
      (Bool i, Bool j) -> Bool (i && j)
      |_ -> raise (TypeError "expected bool")))
    (* |_ -> raise (TypeError "invalid operator")) *)
  |If(e1, e2, e3) -> (match eval_expr env e1 with 
    Bool(true) -> eval_expr env e2
    | Bool(false) -> eval_expr env e3
    |_ -> raise (TypeError "expected bool"))
  |Let(var, b, e1, e2) -> if b then 
      let env1 = extend_tmp env var in
      let v = eval_expr env1 e1 in
      let updt = update env1 var v in
     (match updt with _ ->
      eval_expr env1 e2)
  
    else
      let en = eval_expr env e1 in eval_expr (extend env var en) e2

  |Fun(v, e) -> Closure(env, v, e) 
  |FunctionCall(e1, e2) -> let v = eval_expr env e2 in
    (match eval_expr env e1 with
      Closure(a, x, e) -> eval_expr (extend a x v) e
      |_ -> raise (TypeError "not a function")
  
  )

  (* |_ -> raise (TypeError "invalid expression") *)




(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  Def(var, e) -> (let env1 = extend_tmp env var in
    let v = eval_expr env1 e in
    let updt = update env1 var v in
    match updt with _ ->
    (env1, Some v))
  |Expr(e) -> (env, Some (eval_expr env e))
  |NoOp -> ([], None)
  (* |_ -> raise (TypeError "invalid mutop directive") *)