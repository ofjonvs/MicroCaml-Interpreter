open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)


let rec find_index x lst =
  match lst with
  | [] -> raise (InvalidInputException "no index found")
  | h :: t -> if x = h then 0 else 1 + find_index x t

let rec sublist b e l = 
  match l with
    [] -> raise (InvalidInputException "invalid sublist")
  | h :: t -> 
      let tail = if e=0 then [] else sublist (b-1) (e-1) t in
      if b>0 then tail else h :: tail

let rec parse_expr toks = 
  let (t, exp) = parse_E toks in
  if t <> [] then
    raise (InvalidInputException "end of file not reached")
  else
    (t, exp)

and parse_E toks =
    if lookahead toks = (Some Tok_Let) then parse_let toks 
    else if lookahead toks = (Some Tok_If) then parse_if toks
    else if lookahead toks = (Some Tok_Fun) then parse_fun toks
    else parse_or toks


and parse_let toks =
  let t = match_token toks Tok_Let in 
  let (t', m) = parse_rec t in
  match lookahead t' with
    Some (Tok_ID i) -> 
      let t'' = match_many t' ([Tok_ID i; Tok_Equal]) in
      let in_index = find_index Tok_In t'' in
      let (t''', e1) = parse_E (sublist 0 (in_index - 1) t'') in
      let (t'''', e2) = parse_E (sublist (in_index + 1) ((List.length t'') - 1) t'') in
      ([], Let (i, m, e1, e2))
    |_ -> raise (InvalidInputException "invalid let expression")

and parse_rec toks =
  if lookahead toks = (Some Tok_Rec) then ((match_token toks Tok_Rec), true)
  else toks, false

and parse_if toks = 
  let t = match_token toks Tok_If in 
  let then_index = find_index Tok_Then t in
  let else_index = find_index Tok_Else t in
  let (t', e1) = parse_E (sublist 0 (then_index - 1) t) in
  let (t'', e2) = parse_E (sublist (then_index + 1) (else_index - 1) t) in
  let (t''', e3) = parse_E (sublist (else_index + 1) ((List.length t) - 1) t) in
  ([], If (e1, e2, e3))

and parse_fun toks = 
  let t = match_token toks Tok_Fun in 
  match lookahead t with
  Some (Tok_ID i) ->
    let (t', e1) = parse_E (match_many t ([Tok_ID i; Tok_Arrow])) in
    ([], Fun (i, e1))
  |_ -> raise (InvalidInputException "invalid fun input")

and parse_or toks =
  let (t, m) = parse_and toks in
  if lookahead t = Some Tok_Or then
    let (t', m') = parse_or (match_token t Tok_Or) in
    (t', Binop (Or, m, m'))
  else t, m

and parse_and toks =
  let(t, m) = parse_equality toks in
  if lookahead t = Some Tok_And then
    let (t', m') = parse_and (match_token t Tok_And) in
    (t', Binop (And, m, m'))
  else t, m

and parse_equality toks =
  let (t, m) = parse_relation toks in
  match lookahead t with
  Some Tok_Equal -> let (t', m') = parse_equality (match_token t Tok_Equal) in
    (t',Binop (Equal, m, m'))
  |Some Tok_NotEqual ->  let (t', m') = parse_equality (match_token t Tok_NotEqual) in
    (t', Binop (NotEqual, m, m'))
  |_ -> t, m

and parse_relation toks =
  let (t, m) = parse_add toks in
  match lookahead t with
  Some Tok_Greater -> let (t' , m') = parse_relation (match_token t Tok_Greater) in
    (t', Binop(Greater, m, m'))
  |Some Tok_Less -> let (t', m') = parse_relation (match_token t Tok_Less) in
    (t', Binop (Less, m, m'))
  |Some Tok_GreaterEqual -> let (t', m') = parse_relation (match_token t Tok_GreaterEqual) in
    (t', Binop (GreaterEqual, m, m'))
  |Some Tok_LessEqual -> let (t', m') = parse_relation (match_token t Tok_LessEqual) in
    (t', Binop (LessEqual, m, m'))
  |_ -> t, m

and parse_add toks =
  let (t, m) = parse_mul toks in
  match lookahead t with
  Some Tok_Add -> let (t', m') = parse_add (match_token t Tok_Add) in
    (t', Binop (Add, m, m'))
  |Some Tok_Sub -> let (t', m') = parse_add (match_token t Tok_Sub) in
    (t', Binop (Sub, m, m'))
  |_ -> t, m

and parse_mul toks =
  let (t, m) = parse_concat toks in
  match lookahead t with
  Some Tok_Mult -> let (t', m') = parse_mul (match_token t Tok_Mult) in
    (t', Binop (Mult, m, m'))
  |Some Tok_Div -> let (t', m') = parse_mul (match_token t Tok_Div) in
    (t', Binop (Div, m, m'))
  |_ -> t, m

and parse_concat toks = 
  let (t, m) = parse_unary toks in
  if lookahead t = Some Tok_Concat then
    let (t', m') = parse_concat (match_token t Tok_Concat) in
    (t', Binop(Concat, m, m'))
  else t, m

and parse_unary toks =
  if lookahead toks = Some Tok_Not then
    let (t, m) = parse_unary (match_token toks Tok_Not) in
    (t, Not(m))
  else parse_funcall toks

and parse_funcall toks =
  let (t, m) = parse_prim toks in
  match lookahead t with 
  Some Tok_Int(i) -> let (t', m') = parse_prim t in 
    let (t'', m'') = match lookahead t' with
      Some Tok_Int(i) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
      |Some Tok_Bool(b) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
      |Some Tok_String(s) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
      |Some Tok_ID(id) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
      |Some Tok_LParen -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
      |_ -> t',m' in
    (t'', FunctionCall(m, m''))
  |Some Tok_Bool(b) -> let (t', m') = parse_prim t in 
    let (t'', m'') = match lookahead t' with
        Some Tok_Int(i) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
        |Some Tok_Bool(b) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
        |Some Tok_String(s) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
        |Some Tok_ID(id) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
        |Some Tok_LParen -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
        |_ -> t',m' in
      (t'', FunctionCall(m, m''))
  |Some Tok_String(s) -> let (t', m') = parse_prim t in 
    let (t'', m'') = match lookahead t' with
          Some Tok_Int(i) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
          |Some Tok_Bool(b) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
          |Some Tok_String(s) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
          |Some Tok_ID(id) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
          |Some Tok_LParen -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
          |_ -> t',m' in
        (t'', FunctionCall(m, m''))
  |Some Tok_ID(id) -> let (t', m') = parse_prim t in 
    let (t'', m'') = match lookahead t' with
      Some Tok_Int(i) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
      |Some Tok_Bool(b) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
      |Some Tok_String(s) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
      |Some Tok_ID(id) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
      |Some Tok_LParen -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
      |_ -> t',m' in
    (t'', FunctionCall(m, m''))
  |Some Tok_LParen -> let (t', m') = parse_prim t in 
  let (t'', m'') = match lookahead t' with
        Some Tok_Int(i) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
        |Some Tok_Bool(b) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
        |Some Tok_String(s) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
        |Some Tok_ID(id) -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
        |Some Tok_LParen -> let (t''', m''') = parse_prim t' in (t''', FunctionCall(m', m'''))
        |_ -> t',m' in
      (t'', FunctionCall(m, m''))
  |_ -> t,m

and parse_prim toks =
  match lookahead toks with
   Some Tok_Int(i) -> let t = match_token toks (Tok_Int i) in
    t, Value (Int i)
  |Some Tok_Bool(b) -> let t = match_token toks (Tok_Bool b) in
    t, Value (Bool b)
  |Some Tok_String(s) -> let t = match_token toks (Tok_String s) in
    t, Value (String s)
  |Some Tok_ID(id) -> let t = match_token toks (Tok_ID id) in
     t, ID(id)
  |Some Tok_LParen -> let t = match_token toks Tok_LParen in
    let (t', m) = parse_E t in
    let t'' = match_token t' Tok_RParen in
    t'', m
  |_ -> raise (InvalidInputException "invalid type")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match toks with [Tok_DoubleSemi] -> ([], NoOp)
  |_ ->
  if lookahead toks = Some Tok_Def then
    let t = match_token toks Tok_Def in
    match lookahead t with 
    Some Tok_ID(i) ->
      let t' = match_token t (Tok_ID(i)) in
      if lookahead t' = Some Tok_Equal then
        let t'' = match_token t' Tok_Equal in
        if lookahead t'' = Some Tok_Def then raise (InvalidInputException "Oh no!") else
        let (t''', ast) = parse_expr (sublist 0 ((List.length t'') - 2) t'') in
        if (sublist (List.length t'' - 1) (List.length t'' - 1) t'') = [Tok_DoubleSemi] then
        ([], Def(i, ast))
        else raise (InvalidInputException "no double semicolon found")
      else raise (InvalidInputException "invalid input (no equal sign found")
    |_ -> raise (InvalidInputException "invalid id")

    else if (sublist (List.length toks - 1) (List.length toks - 1) toks) = [Tok_DoubleSemi] then
      let (t, ast) = parse_expr (sublist 0 ((List.length toks) - 2) toks) in
      ([], Expr(ast))
    else raise (InvalidInputException "no double semicolon found")