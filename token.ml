open Printf

let gobble_tokens lb =
  let toks = ref [] in
  let rec aux lb =
    try
      let tok = Lexer.tokenize lb in
      toks := tok :: !toks;
      aux lb
    with
    | Lexer.Eof | Exit ->
      List.rev (!toks)
    | Lexer.UnterminatedString s ->
      printf "Unterminated string: %s\n" s;
      exit 1
    | Lexer.TokenError c ->
      printf "Tokenizer error: %c\n" c;
      exit 1
  in aux lb

let dump_tokens toks =
  printf "\nTokens\n------\n";
  let rec aux toks i =
    match toks with
    | [] -> ()
    | t::ts ->
      printf "<%d> %s\n" i (Lexer.string_of_token t#kind);
      aux ts (i + 1)
  in aux toks 1
