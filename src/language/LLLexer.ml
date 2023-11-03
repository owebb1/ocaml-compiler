(*
This file defines a lexer for an LL parser generator for Pyrrhuloxia.
*)

open Batteries;;

(** An exception type which should be raised when lexing fails. *)
exception LexerError of string;;

(** The type of tokens produced by this lexer. *)
type token =
  | TokInt of int
  | TokOpenParen
  | TokCloseParen
  | TokBool of bool
  | TokAfter
  | TokBefore
  | TokPrint
  | TokIsBool
  | TokIsInt
  | TokPlus
  | TokMinus
  | TokAsterik
  | TokLessThan
  | TokGreaterThan
  | TokEqual
  | TokAnd
  | TokOr
  | TokLet
  | TokIn
  | TokIf
  | TokThen
  | TokElse
  | TokIdentifier of string
  (* TODO: add more token types here *)
[@@deriving eq, ord, show];;

(** A helper function which attempts to lex the prefix of a string into a token.
    It takes as an argument a list of mini-lexers: functions which *try* to
    recognize a specific token and raise a LexerError if they fail. *)
let rec tokenize_first_of
    (tokenizers : (char list -> token * char list) list)
    (input : char list)
  : token * char list =
  match tokenizers with
  | [] ->
    (* We have no routines to use to find a token.  Give up, producing an error
       message to hint to the user where we were in the file. *)
    let max_error_length = 20 in
    let first_part_of_string =
      String.of_list (List.take max_error_length input)
    in
    raise (LexerError(Printf.sprintf "Unrecognized token starting at: \"%s\""
                        first_part_of_string))
  | tokenizer :: tokenizers' ->
    try
      (* If this tokenizer successfully produces a result, use it! *)
      tokenizer input
    with
    | LexerError _ ->
      (* This tokenizer failed.  Let's try the next one. *)
      tokenize_first_of tokenizers' input
;;

(** This routine discards whitespace from the input stream. *)
let discard_whitespace (input : char list) : char list =
  List.drop_while Char.is_whitespace input
;;

let identifier_char (c : char) : bool = 
  (Char.is_digit c || Char.is_letter c || c == '_' || c == '\'') 
;;

let operand_char (c : char) : bool =
  (c == ')' || c == '+' || c == '-' || c == '*' || c == '<' || c == '>' || c == '=' || c == '&' || c == '|')
;;

(** This routine attempts to lex a single numeric token from the input.
    Note that this routine does NOT handle negative numbers. *)
let tokenize_int (input : char list) : token * char list =
  (* Get all of the digits from the front of the input that we can. *)
  let digits = List.take_while Char.is_digit input in
  let rest = List.drop_while Char.is_digit input in
  if not (List.is_empty rest) then 
    let first_of_rest = String.get (String.of_list rest) 0 in

    if not ((operand_char first_of_rest) || (Char.is_whitespace first_of_rest )) then
      raise (LexerError "Could not tokenize integer")
    else
      (* If we didn't get any digits, then the next token isn't an integer. *)
    if List.is_empty digits then
      raise (LexerError "Could not tokenize integer")
    else

      (* Convert the list of digits into a string. *)
      let digit_string = String.of_list digits in
      (* Turn that into a number. *)
      let number = int_of_string digit_string in
      (* Return a token with that number along with the rest of the input. *)
      (TokInt number, rest)
  else 
    let digit_string = String.of_list digits in
    (* Turn that into a number. *)
    let number = int_of_string digit_string in
    (* Return a token with that number along with the rest of the input. *)
    (TokInt number, [])
;;

(** This routine attempts to lex a single plus token from the input. *)
let tokenize_plus (input : char list) : token * char list =
  match input with
  | '+'::rest -> (TokPlus, rest)
  | _ -> raise (LexerError "Could not tokenize plus")
;;

(** This routine attemps to lex a single open parenthesis from the input. *)
let tokenize_open_paren (input : char list) : token * char list =
  match input with
  | '('::rest -> (TokOpenParen, rest)
  | _ -> raise (LexerError "Could not tokenize open parenthesis")
;;

(** This routine attemps to lex a single close parenthesis from the input. *)
let tokenize_close_paren (input : char list) : token * char list =
  match input with
  | ')'::rest -> (TokCloseParen, rest)
  | _ -> raise (LexerError "Could not tokenize close parenthesis")
;;

(* TODO: write more lexer routines here *)
let tokenize_bool (input : char list) : token * char list =
  match input with
  | 't'::'r'::'u'::'e'::[] -> 
    (TokBool(true), [])
  | 'f'::'a'::'l'::'s'::'e'::[] ->
    (TokBool(false),[])
  | 't'::'r'::'u'::'e'::next::rest
    when (not (identifier_char next)) ->
    (TokBool(true), next::rest)
  | 'f'::'a'::'l'::'s'::'e'::next::rest
    when (not (identifier_char next)) ->
    (TokBool(false), next::rest)
  | _ -> raise (LexerError "Could not tokenize boolean")
;;

let tokenize_after (input : char list) : token * char list =
  match input with
  | 'a'::'f'::'t'::'e'::'r'::[] -> 
    (TokAfter, [])
  | 'a'::'f'::'t'::'e'::'r'::next::rest
    when (not (identifier_char next)) ->
    (TokAfter, next::rest)
  | _ -> raise (LexerError "Could not tokenize after")
;;

let tokenize_before (input : char list) : token * char list =
  match input with
  | 'b'::'e'::'f'::'o'::'r'::'e'::[] -> 
    (TokBefore, [])
  | 'b'::'e'::'f'::'o'::'r'::'e'::next::rest
    when (not (identifier_char next)) ->
    (TokBefore, next::rest)
  | _ -> raise (LexerError "Could not tokenize before")
;;

let tokenize_print (input : char list) : token * char list = 
  match input with
  | 'p'::'r'::'i'::'n'::'t'::[] -> 
    (TokPrint, [])
  | 'p'::'r'::'i'::'n'::'t'::next::rest
    when (not (identifier_char next)) ->
    (TokPrint, next::rest)
  | _ -> raise (LexerError "Could not tokenize print")
;;

let tokenize_isint (input : char list) : token * char list = 
  match input with
  | 'i'::'s'::'i'::'n'::'t'::[] -> 
    (TokIsInt, [])
  | 'i'::'s'::'i'::'n'::'t'::next::rest
    when (not (identifier_char next)) ->
    (TokIsInt, next::rest)
  | _ -> raise (LexerError "Could not tokenize isint")
;;

let tokenize_isbool (input : char list) : token * char list = 
  match input with
  | 'i'::'s'::'b'::'o'::'o'::'l'::[] -> 
    (TokIsBool, [])
  | 'i'::'s'::'b'::'o'::'o'::'l'::next::rest
    when (not (identifier_char next)) ->
    (TokIsBool, next::rest)
  | _ -> raise (LexerError "Could not tokenize isbool")
;;

let tokenize_minus (input : char list) : token * char list =
  match input with
  | '-'::rest -> (TokMinus, rest)
  | _ -> raise (LexerError "Could not tokenize minus")
;;


let tokenize_asterik (input : char list) : token * char list =
  match input with
  | '*'::rest -> (TokAsterik, rest)
  | _ -> raise (LexerError "Could not tokenize asterik")
;;

let tokenize_less_than (input : char list) : token * char list =
  match input with
  | '<'::rest -> (TokLessThan, rest)
  | _ -> raise (LexerError "Could not tokenize less than")
;;

let tokenize_greater_than (input : char list) : token * char list =
  match input with
  | '>'::rest -> (TokGreaterThan, rest)
  | _ -> raise (LexerError "Could not tokenize greater than")
;;

let tokenize_equal (input : char list) : token * char list =
  match input with
  | '='::rest -> (TokEqual, rest)
  | _ -> raise (LexerError "Could not tokenize equals")
;;

let tokenize_and (input : char list) : token * char list =
  match input with
  | '&'::'&'::rest -> (TokAnd, rest)
  | _ -> raise (LexerError "Could not tokenize double amprisand")
;;

let tokenize_or (input : char list) : token * char list =
  match input with
  | '|'::'|'::rest -> (TokOr, rest)
  | _ -> raise (LexerError "Could not tokenize double or")
;;

let tokenize_let (input : char list) : token * char list =
  match input with
  | 'l'::'e'::'t'::[] -> 
    (TokLet, [])
  | 'l'::'e'::'t'::next::rest
    when (not (identifier_char next)) ->
    (TokLet, next::rest)
  | _ -> raise (LexerError "Could not tokenize let") 
;;

let tokenize_in (input : char list) : token * char list =
  match input with
  | 'i'::'n'::[] -> 
    (TokIn, [])
  | 'i'::'n'::next::rest
    when (not (identifier_char next)) ->
    (TokIn, next::rest)
  | _ -> raise (LexerError "Could not tokenize in") 
;;

let tokenize_if (input : char list) : token * char list =
  match input with
  | 'i'::'f'::[] -> 
    (TokIf, [])
  | 'i'::'f'::next::rest
    when (not (identifier_char next)) ->
    (TokIf, next::rest)
  | _ -> raise (LexerError "Could not tokenize if") 
;;

let tokenize_then (input : char list) : token * char list =
  match input with
  | 't'::'h'::'e'::'n'::[] -> 
    (TokThen, [])
  | 't'::'h'::'e'::'n'::next::rest
    when (not (identifier_char next)) ->
    (TokThen, next::rest)
  | _ -> raise (LexerError "Could not tokenize then") 
;;

let tokenize_else (input : char list) : token * char list =
  match input with
  | 'e'::'l'::'s'::'e'::[] -> 
    (TokElse, [])
  | 'e'::'l'::'s'::'e'::next::rest
    when (not (identifier_char next)) ->
    (TokElse, next::rest)
  | _ -> raise (LexerError "Could not tokenize else") 
;;



let tokenize_identifier (input : char list) : token * char list = 
  let rec build (iden : char list) (input : char list) : char list = 
    match input with
    | [] -> iden
    | c::_
      when ((Char.is_whitespace c) || (operand_char c)) ->
      iden
    | c::rest
      when (identifier_char c) ->
      let iden' = iden@[c] in
      build iden' rest
    (* if Char.is_whitespace c then
       iden
       else
       when (identifier_char c) -> 
       let iden' = iden::c in
       build iden' rest *)
    | _ -> raise (LexerError "Could not tokenize identifier") 
  in 

  (* check if first item is alphanumeric or underscore, otherwise throw error *)
  match input with
  | [] -> raise (LexerError "Could not tokenize identifier of length 0")
  | c::_ -> 
    if (Char.is_digit c) then 
      raise (LexerError "Could not tokenize identifier")
    else
      let identifier_list = build [] (input) in
      let identifier_string = String.of_list identifier_list in
      let identifier_size = List.length identifier_list in 
      let input_string = String.of_list input in 
      let rest_string = String.tail input_string identifier_size in 
      let rest' : char list = String.to_list rest_string in 
      (TokIdentifier(identifier_string),rest' )
;;








(** This routine attempts to take a single token from the input stream.  If an
    unrecoverable error occurs, a LexerError is raised. *)
let tokenize (input : char list) : token * char list =
  (* TODO: modify this function as you write more lexer routines. *)
  tokenize_first_of
    [ tokenize_int;
      tokenize_plus;
      tokenize_open_paren;
      tokenize_close_paren;
      tokenize_bool;
      tokenize_after;
      tokenize_and;
      tokenize_asterik;
      tokenize_before;
      tokenize_print;
      tokenize_equal;
      tokenize_greater_than;
      tokenize_isbool;
      tokenize_isint;
      tokenize_less_than;
      tokenize_minus;
      tokenize_or;
      tokenize_let;
      tokenize_in;
      tokenize_if;
      tokenize_then;
      tokenize_else;
      tokenize_identifier;
    ]
    input
;;

(** A function to lex a string.  If lexing is successful, a list of tokens is
    returned.  Otherwise, a LexerError is raised. *)
let lex (text : string) : token list =
  let input = String.to_list text in
  (*
    This function recursively takes a token from the input stream.  It builds up
    a list in *reverse* order because this allows it to tail recurse; the
    list is reversed once the routine is complete.
  *)
  let rec take_tokens (tokens_so_far : token list) (input : char list)
    : token list =
    (* Get rid of any leading whitespace. *)
    let input' = discard_whitespace input in
    (* If we're out of input, then return the list of tokens we have. *)
    if List.is_empty input' then
      tokens_so_far
    else
      (* Take a token from the input. *)
      let (token, input'') = tokenize input' in
      (* Recurse to take more tokens.  Note that the new token is put on the
         front of the list, resulting in the list being constructed in backwards
         order.  This is reversed later.  Doing things this way allows us to
         tail recurse here!  :-D *)
      take_tokens (token :: tokens_so_far) input''
  in
  let answer = List.rev (take_tokens [] input) in
  (* print_endline(String.join "\n" (List.map show_token answer)); *)
  answer
;;
