open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

(* let string_of_token tok = match tok with
| Tok_Int(i) -> astring_of_int i
| Tok_Mult -> "*"
| Tok_LParen -> "("
| Tok_RParen -> ")"
| Tok_Equal -> "="
| Tok_NotEqual -> "<>"
| Tok_Greater -> ">"
| Tok_Less -> "<"
| Tok_GreaterEqual -> ">="
| Tok_LessEqual -> "<="
| Tok_Or -> "||"
| Tok_And -> "&&"
| Tok_Not -> "not"
| Tok_If -> "if"
| Tok_Then -> "then"
| Tok_Else -> "else"
| Tok_Add -> "+"
| Tok_Sub -> "-"
| Tok_Div -> "/"
| Tok_Concat -> "^"
| Tok_Let -> "let"
| Tok_Def -> "def"
| Tok_In  -> "in"
| Tok_Rec -> "rec"
| Tok_Fun -> "fun"
| Tok_Arrow -> "->"
| Tok_DoubleSemi -> ";;" *)

let rec tokenize input = 
  let length = String.length input in

  let rec tokeniz pos =
    if pos >= length then
      []

    else if Str.string_match (Str.regexp "[0-9]+") input pos then
      let value = Str.matched_string input in
      Tok_Int(int_of_string value)::(tokeniz (pos + String.length value))

    else if (Str.string_match (Str.regexp "(-[0-9]+)") input pos) then
      let value = Str.matched_string input in
      Tok_Int(int_of_string (String.sub value 1 ((String.length value) - 2)))::(tokeniz (pos + String.length value))

    else if (Str.string_match (Str.regexp {|\"[\"]*\"|}) input pos) then
      failwith "InvalidInputException"

    else if (Str.string_match (Str.regexp {|\"[^\"]*\"|}) input pos) then
      let value = Str.matched_string input in
      let new_val = String.sub value 1 ((String.length value) - 2) in
      Tok_String(new_val)::(tokeniz (pos + String.length value))
    
    else if (Str.string_match (Str.regexp {|\(true\|false\|not\|if\|then\|else\|let\|def\|in\|rec\|fun\)[a-zA-Z0-9]+|}) input pos) then
      let value = Str.matched_string input in
      Tok_ID(value)::(tokeniz (pos + String.length (value)))
    
    else if (Str.string_match (Str.regexp "not") input pos) then
      Tok_Not::(tokeniz (pos + 3))

    else if (Str.string_match (Str.regexp "if") input pos) then
      Tok_If::(tokeniz (pos + 2))

    else if (Str.string_match (Str.regexp "then") input pos) then
      Tok_Then::(tokeniz (pos + 4))

    else if (Str.string_match (Str.regexp "else") input pos) then
      Tok_Else::(tokeniz (pos + 4))

    else if (Str.string_match (Str.regexp "let") input pos) then
      Tok_Let::(tokeniz (pos + 3))

    else if (Str.string_match (Str.regexp "def") input pos) then
      Tok_Def::(tokeniz (pos + 3))

    else if (Str.string_match (Str.regexp "in") input pos) then
      Tok_In::(tokeniz (pos + 2))

    else if (Str.string_match (Str.regexp "rec") input pos) then
      Tok_Rec::(tokeniz (pos + 3))

    else if (Str.string_match (Str.regexp "fun") input pos) then
      Tok_Fun::(tokeniz (pos + 3))

    else if (Str.string_match (Str.regexp "true") input pos) then
      Tok_Bool(true)::(tokeniz (pos + 4))
    
    else if (Str.string_match (Str.regexp "false") input pos) then
      Tok_Bool(false)::(tokeniz (pos + 5))

    else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then
      let value = Str.matched_string input in
      Tok_ID(value)::(tokeniz (pos + String.length (value)))

    else if (Str.string_match (Str.regexp "=") input pos) then
      Tok_Equal::(tokeniz (pos + 1))

    else if (Str.string_match (Str.regexp "<>") input pos) then
      Tok_NotEqual::(tokeniz (pos + 2))

    else if (Str.string_match (Str.regexp ">=") input pos) then
      Tok_GreaterEqual::(tokeniz (pos + 2))

    else if (Str.string_match (Str.regexp "<=") input pos) then
      Tok_LessEqual::(tokeniz (pos + 2))

    else if (Str.string_match (Str.regexp ">") input pos) then
      Tok_Greater::(tokeniz (pos + 1))

    else if (Str.string_match (Str.regexp "<") input pos) then
      Tok_Less::(tokeniz (pos + 1))

    else if (Str.string_match (Str.regexp "||") input pos) then
      Tok_Or::(tokeniz (pos + 2))

    else if (Str.string_match (Str.regexp "&&") input pos) then
      Tok_And::(tokeniz (pos + 2))

    else if (Str.string_match (Str.regexp "->") input pos) then
      Tok_Arrow::(tokeniz (pos + 2))

    else if (Str.string_match (Str.regexp "-") input pos) then
      Tok_Sub::(tokeniz (pos + 1))

    else if (Str.string_match (Str.regexp "/") input pos) then
      Tok_Div::(tokeniz (pos + 1))

    else if (Str.string_match (Str.regexp "\\^") input pos) then
      Tok_Concat::(tokeniz (pos + 1))

    else if (Str.string_match (Str.regexp ";;") input pos) then
      Tok_DoubleSemi::(tokeniz (pos + 2))

    else if Str.string_match (Str.regexp "(") input pos then
      Tok_LParen::(tokeniz (pos + 1))

    else if Str.string_match (Str.regexp ")") input pos then
      Tok_RParen::(tokeniz (pos + 1))

    else if Str.string_match (Str.regexp "\\+") input pos then
      Tok_Add::(tokeniz (pos + 1))

    else if Str.string_match (Str.regexp "\\*") input pos then
      Tok_Mult::(tokeniz (pos + 1))

    else
      tokeniz (pos + 1)

  in tokeniz 0;;