
{
open Flambda_parser
open Lexing

type location = Lexing.position * Lexing.position

type error =
  | Illegal_character of char
  | Invalid_literal of string
  | No_such_primitive of string
  | Unterminated_string
  | Unterminated_string_in_comment
  | Unterminated_comment
  | Illegal_escape of string * string option
;;

let pp_error ppf = function
  | Illegal_character c -> Format.fprintf ppf "Illegal character %c" c
  | Invalid_literal s -> Format.fprintf ppf "Invalid literal %s" s
  | No_such_primitive s -> Format.fprintf ppf "No such primitive %%%s" s
  | Unterminated_string -> Format.fprintf ppf "Unterminated string"
  | Unterminated_string_in_comment ->
     Format.fprintf ppf "Unterminated string in comment"
  | Unterminated_comment -> Format.fprintf ppf "Unterminated comment"
  | Illegal_escape (s, explanation) ->
     Format.fprintf ppf
        "Illegal backslash escape in string or character (%s)%t" s
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> Format.fprintf ppf ": %s" expl)

exception Error of error * location;;

let current_location lexbuf =
  (Lexing.lexeme_start_p lexbuf,
   Lexing.lexeme_end_p lexbuf)

let error ~lexbuf e = raise (Error (e, current_location lexbuf))

let create_hashtable init =
  let tbl = Hashtbl.create (List.length init) in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let keyword_table =
  create_hashtable [
    "always", KWD_ALWAYS;
    "and", KWD_AND;
    "andwhere", KWD_ANDWHERE;
    "any", KWD_ANY;
    "apply", KWD_APPLY;
    "array", KWD_ARRAY;
    "asr", KWD_ASR;
    "available", KWD_AVAILABLE;
    "boxed", KWD_BOXED;
    "bswap", KWD_BSWAP;
    "ccall", KWD_CCALL;
    "closure", KWD_CLOSURE;
    "code", KWD_CODE;
    "cont", KWD_CONT;
    "default", KWD_DEFAULT;
    "define_root_symbol", KWD_DEFINE_ROOT_SYMBOL;
    "deleted", KWD_DELETED;
    "depth", KWD_DEPTH;
    "direct", KWD_DIRECT;
    "done", KWD_DONE;
    "end", KWD_END;
    "error", KWD_ERROR;
    "exn", KWD_EXN;
    "float", KWD_FLOAT;
    "halt_and_catch_fire", KWD_HCF;
    "heap_or_local", KWD_HEAP_OR_LOCAL;
    "hint", KWD_HINT;
    "id", KWD_ID;
    "imm", KWD_IMM;
    "immutable_unique", KWD_IMMUTABLE_UNIQUE;
    "in", KWD_IN;
    "inf", KWD_INF;
    "inline", KWD_INLINE;
    "inlined", KWD_INLINED;
    "inlining_state", KWD_INLINING_STATE;
    "int32", KWD_INT32;
    "int64", KWD_INT64;
    "invalid", KWD_INVALID;
    "land", KWD_LAND;
    "let", KWD_LET;
    "local", KWD_LOCAL;
    "loopify", KWD_LOOPIFY;
    "lsl", KWD_LSL;
    "lsr", KWD_LSR;
    "mutable", KWD_MUTABLE;
    "nativeint", KWD_NATIVEINT;
    "never", KWD_NEVER;
    "newer_version_of", KWD_NEWER_VERSION_OF;
    "noalloc", KWD_NOALLOC;
    "notrace", KWD_NOTRACE;
    "of", KWD_OF;
    "pop", KWD_POP;
    "push", KWD_PUSH;
    "rec", KWD_REC;
    "rec_info", KWD_REC_INFO;
    "region", KWD_REGION;
    "regular", KWD_REGULAR;
    "reraise", KWD_RERAISE;
    "set_of_closures", KWD_SET_OF_CLOSURES;
    "size", KWD_SIZE;
    "succ", KWD_SUCC;
    "switch", KWD_SWITCH;
    "tag", KWD_TAG;
    "tagged", KWD_TAGGED;
    "tailrec", KWD_TAILREC;
    "toplevel", KWD_TOPLEVEL;
    "tupled", KWD_TUPLED;
    "unit", KWD_UNIT;
    "unreachable", KWD_UNREACHABLE;
    "unroll", KWD_UNROLL;
    "unsigned", KWD_UNSIGNED;
    "val", KWD_VAL;
    "where", KWD_WHERE;
    "with", KWD_WITH;

    (* Constructors for static constants *)
    "Block", STATIC_CONST_BLOCK;
    "Float_array", STATIC_CONST_FLOAT_ARRAY;
    "Float_block", STATIC_CONST_FLOAT_BLOCK;
    "Empty_array", STATIC_CONST_EMPTY_ARRAY;
]

let ident_or_keyword str =
  try Hashtbl.find keyword_table str
  with Not_found -> IDENT str

let is_keyword str =
  Hashtbl.mem keyword_table str

let prim_table =
  create_hashtable [
    "array_length", PRIM_ARRAY_LENGTH;
    "array_load", PRIM_ARRAY_LOAD;
    "array_set", PRIM_ARRAY_SET;
    "begin_region", PRIM_BEGIN_REGION;
    "begin_try_region", PRIM_BEGIN_TRY_REGION;
    "bigstring_load", PRIM_BIGSTRING_LOAD;
    "bigstring_set", PRIM_BIGSTRING_SET;
    "Block", PRIM_BLOCK;
    "block_load", PRIM_BLOCK_LOAD;
    "block_set", PRIM_BLOCK_SET;
    "not", PRIM_BOOLEAN_NOT;
    "Box_float", PRIM_BOX_FLOAT;
    "Box_int32", PRIM_BOX_INT32;
    "Box_int64", PRIM_BOX_INT64;
    "Box_nativeint", PRIM_BOX_NATIVEINT;
    "bytes_length", PRIM_BYTES_LENGTH;
    "bytes_load", PRIM_BYTES_LOAD;
    "bytes_set", PRIM_BYTES_SET;
    "end_region", PRIM_END_REGION;
    "get_tag", PRIM_GET_TAG;
    "int_arith", PRIM_INT_ARITH;
    "int_comp", PRIM_INT_COMP;
    "int_shift", PRIM_INT_SHIFT;
    "is_flat_float_array", PRIM_IS_FLAT_FLOAT_ARRAY;
    "is_int", PRIM_IS_INT;
    "num_conv", PRIM_NUM_CONV;
    "Opaque", PRIM_OPAQUE;
    "phys_eq", PRIM_PHYS_EQ;
    "phys_ne", PRIM_PHYS_NE;
    "project_value_slot", PRIM_PROJECT_VALUE_SLOT;
    "project_function_slot", PRIM_PROJECT_FUNCTION_SLOT;
    "string_length", PRIM_STRING_LENGTH;
    "string_load", PRIM_STRING_LOAD;
    "Tag_imm", PRIM_TAG_IMM;
    "unbox_float", PRIM_UNBOX_FLOAT;
    "unbox_int32", PRIM_UNBOX_INT32;
    "unbox_int64", PRIM_UNBOX_INT64;
    "unbox_nativeint", PRIM_UNBOX_NATIVEINT;
    "untag_imm", PRIM_UNTAG_IMM;
]

let prim ~lexbuf str =
  try Hashtbl.find prim_table str
  with Not_found -> error ~lexbuf (No_such_primitive str)

let unquote_ident str =
  match str with
  | "" -> ""
  | _ ->
    begin
      match String.get str 0 with
      | '`' -> String.sub str 1 (String.length str - 2)
      | _ -> str
    end

let symbol cunit_ident cunit_linkage_name ident =
  let cunit =
    Option.map (fun cunit_ident ->
      { Fexpr.ident = unquote_ident cunit_ident;
        linkage_name = Option.map unquote_ident cunit_linkage_name }
    ) cunit_ident
  in
  SYMBOL (cunit, unquote_ident ident)

(**** Begin comment and string code adapted from the ocaml lexer ****)
let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer

let store_string_char c = Buffer.add_char string_buffer c
let store_string_utf_8_uchar u = Buffer.add_utf_8_uchar string_buffer u
let store_string s = Buffer.add_string string_buffer s
let store_lexeme lexbuf = store_string (Lexing.lexeme lexbuf)

(* To store the position of the beginning of a string and comment *)
let comment_depth = ref 0;;
let in_comment () = !comment_depth <> 0;;
let is_in_string = ref false

(* Escaped chars are interpreted in strings unless they are in comments. *)
let store_escaped_char lexbuf c =
  if in_comment () then store_lexeme lexbuf else store_string_char c

let store_escaped_uchar lexbuf u =
  if in_comment () then store_lexeme lexbuf else store_string_utf_8_uchar u

let wrap_string_lexer f lexbuf =
  reset_string_buffer();
  is_in_string := true;
  let string_start = lexbuf.lex_start_p in
  let _ = f lexbuf in
  is_in_string := false;
  lexbuf.lex_start_p <- string_start;
  get_stored_string ()

let wrap_comment_lexer comment lexbuf =
  comment_depth := 1;
  reset_string_buffer ();
  let _ = comment lexbuf in
  let s = get_stored_string () in
  reset_string_buffer ();
  s

(* to translate escape sequences *)
let digit_value c =
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false

let num_value lexbuf ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    let v = digit_value (Lexing.lexeme_char lexbuf i) in
    assert(v < base);
    c := (base * !c) + v
  done;
  !c

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let illegal_escape lexbuf reason =
  let e = Illegal_escape (Lexing.lexeme lexbuf, Some reason) in
  error ~lexbuf e

let char_for_decimal_code lexbuf i =
  let c = num_value lexbuf ~base:10 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    if in_comment ()
    then 'x'
    else
      illegal_escape lexbuf
        (Printf.sprintf
          "%d is outside the range of legal characters (0-255)." c)
  else Char.chr c

let char_for_octal_code lexbuf i =
  let c = num_value lexbuf ~base:8 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    if in_comment ()
    then 'x'
    else
      illegal_escape lexbuf
        (Printf.sprintf
          "o%o (=%d) is outside the range of legal characters (0-255)." c c)
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  Char.chr (num_value lexbuf ~base:16 ~first:i ~last:(i+1))

let uchar_for_uchar_escape lexbuf =
  let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
  let first = 3 (* skip opening \u{ *) in
  let last = len - 2 (* skip closing } *) in
  let digit_count = last - first + 1 in
  match digit_count > 6 with
  | true ->
      illegal_escape lexbuf
        "too many digits, expected 1 to 6 hexadecimal digits"
  | false ->
      let cp = num_value lexbuf ~base:16 ~first ~last in
      if Uchar.is_valid cp then Uchar.unsafe_of_int cp else
      illegal_escape lexbuf
        (Printf.sprintf "%X is not a Unicode scalar value" cp)
(**** End comment and string code taken from the ocaml lexer ****)

}

let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identstart = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let ident = identstart identchar*
let quoted_ident = '`' [^ '`' '\n']* '`'
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let sign = ['-']
let int_literal =
  sign? (decimal_literal | hex_literal | oct_literal | bin_literal)
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let int_modifier = ['G'-'Z' 'g'-'z']
let newline = ('\013'* '\010')

rule token = parse
  | "\n"
      { Lexing.new_line lexbuf; token lexbuf }
  | blank +
      { token lexbuf }
  | ":"
      { COLON }
  | ","
      { COMMA }
  | "."
      { DOT }
  | ";"
      { SEMICOLON }
  | "="
      { EQUAL }
  | "_"
      { BLANK }
  | "{"
      { LBRACE }
  | "}"
      { RBRACE }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "["
      { LBRACK }
  | "]"
      { RBRACK }
  | "[|"
      { LBRACKPIPE }
  | "|]"
      { RBRACKPIPE }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { STAR }
  | "/"  { SLASH }
  | "%"  { PERCENT }
  | "<"  { LESS }
  | ">"  { GREATER }
  | "<=" { LESSEQUAL }
  | ">=" { GREATEREQUAL }
  | "<>" { NOTEQUAL }
  | "?"  { QMARK }
  | "+." { PLUSDOT }
  | "-." { MINUSDOT }
  | "*." { STARDOT }
  | "/." { SLASHDOT }
  | "=." { EQUALDOT }
  | "<>." { NOTEQUALDOT }
  | "<." { LESSDOT }
  | "<=." { LESSEQUALDOT }
  | "?." { QMARKDOT }
  | "<-" { LESSMINUS }
  | "->" { MINUSGREATER }
  | "@" { AT }
  | "|"  { PIPE }
  | "~"  { TILDE }
  | "~-"  { TILDEMINUS }
  | "&"  { AMP }
  | "^"  { CARET }
  | "===>" { BIGARROW }
  | ident as ident
         { ident_or_keyword ident }
  | quoted_ident as ident
         { IDENT (unquote_ident ident) }
  | '$'
    (((identchar* | quoted_ident) as cunit_ident)
     ('/' ((identchar* | quoted_ident) as cunit_linkage_name))?
     '.')?
    ((identchar* | quoted_ident) as ident)
         { symbol cunit_ident cunit_linkage_name ident }
  | '%' (identchar+ as p)
         { prim ~lexbuf p }
  | (int_literal as lit) (int_modifier as modif)?
         { INT (lit, modif) }
  | float_literal | hex_float_literal as lit
         { FLOAT (lit |> Float.of_string) }
  | (float_literal | hex_float_literal | int_literal) identchar+ as lit
         { error ~lexbuf (Invalid_literal lit) }

  (* Comment and string code taken from the ocaml lexer *)
  | "\""
      { let s = wrap_string_lexer string lexbuf in
        STRING s }
  | "(*"
      { let _ = wrap_comment_lexer comment lexbuf in
        token lexbuf }

  | eof  { EOF }
  | _ as ch
         { error ~lexbuf (Illegal_character ch) }

(* Comment and string code taken from the ocaml lexer *)
and comment = parse
    "(*"
      { comment_depth := 1 + !comment_depth;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "*)"
      { match !comment_depth with
        | 0 -> assert false
        | 1 -> comment_depth := 0; ()
        | n -> comment_depth := n-1;
               store_lexeme lexbuf;
               comment lexbuf
       }
  | "\""
      {
        store_string_char '\"';
        is_in_string := true;
        let _ = try string lexbuf
        with Error (Unterminated_string, _) ->
          match !comment_depth with
          | 0 -> assert false
          | _ ->
            comment_depth := 0;
            error ~lexbuf Unterminated_string_in_comment
        in
        is_in_string := false;
        store_string_char '\"';
        comment lexbuf }
  | "\'\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'" newline "\'"
      { update_loc lexbuf None 1 false 1;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "\'" [^ '\\' '\'' '\010' '\013' ] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" ['\\' '\"' '\'' 'n' 't' 'b' 'r' ' '] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | eof
      { match !comment_depth with
        | 0 -> assert false
        | _ ->
          comment_depth := 0;
          error ~lexbuf Unterminated_comment
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | ident
      { store_lexeme lexbuf; comment lexbuf }
  | _
      { store_lexeme lexbuf; comment lexbuf }

(* Comment and string code taken from the ocaml lexer *)
and string = parse
    '\"'
      { lexbuf.lex_start_p }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        if in_comment () then store_lexeme lexbuf;
        string lexbuf
      }
  | '\\' (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c)
      { store_escaped_char lexbuf (char_for_backslash c);
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_escaped_char lexbuf (char_for_decimal_code lexbuf 1);
         string lexbuf }
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7']
      { store_escaped_char lexbuf (char_for_octal_code lexbuf 2);
         string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_escaped_char lexbuf (char_for_hexadecimal_code lexbuf 2);
         string lexbuf }
  | '\\' 'u' '{' hex_digit+ '}'
        { store_escaped_uchar lexbuf (uchar_for_uchar_escape lexbuf);
          string lexbuf }
  | '\\' _
      { 
        store_lexeme lexbuf;
        string lexbuf
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        string lexbuf
      }
  | eof
      { is_in_string := false;
        error ~lexbuf Unterminated_string }
  | (_ as c)
      { store_string_char c;
        string lexbuf }
