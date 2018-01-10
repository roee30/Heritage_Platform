(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* A simple lexer recognizing idents, integers, punctuation symbols,
   and skipping spaces and comments between % and eol.
   The transliteration scheme is Velthuis with aa for long a etc. *)

module Skt_lexer = struct

open Camlp4.PreCast;
open Format;

  module Loc = Loc; (* Using the PreCast Loc *)
  module Error = struct
    type t = string;
    exception E of t;
    value to_string x = x;
    value print = Format.pp_print_string;
  end;
  module Token = struct
    module Loc = Loc; 
    type t =
      [ KEYWORD of string
      | IDENT of string 
      | INT of int
      | EOI
      ]
    ;
    module Error = Error;
    module Filter = struct
      type token_filter = Camlp4.Sig.stream_filter t Loc.t
      ;
      type t = string -> bool
      ;
      value mk is_kwd = is_kwd
      ;
      value rec filter is_kwd = parser
        [ [: `((KEYWORD s, loc) as p); strm :] ->
             if is_kwd s then [: `p; filter is_kwd strm :]
             else raise (Encode.In_error ("Undefined token : " ^ s))
        | [: `x; s :] -> [: `x; filter is_kwd s :]
        | [: :] -> [: :] 
        ] 
      ;
      value define_filter _ _ = ()
      ;
      value keyword_added _ _ _ = ()
      ;
      value keyword_removed _ _ = ()
      ;
    end
    ;
    value to_string = fun
      [ KEYWORD s -> sprintf "KEYWORD %S" s
      | IDENT s -> sprintf "IDENT %S" s
      | INT i -> sprintf "INT %d" i
      | EOI -> "EOI"
      ]
    ;
    value print ppf x = pp_print_string ppf (to_string x)
    ;
    value match_keyword kwd = fun
      [ KEYWORD kwd' when kwd' = kwd -> True
      | _ -> False
      ]
    ;
    value extract_string = fun
      [ INT i -> string_of_int i
      | IDENT s | KEYWORD s -> s
      | EOI -> "" 
      ]
    ;
  end
  ;

  open Token
  ;

(* The string buffering machinery - ddr + np *)
value store buf c = do { Buffer.add_char buf c; buf }
;
value rec number buf =
  parser
  [ [: `('0'..'9' as c); s :] -> number (store buf c) s
  | [: :] -> Buffer.contents buf
  ]
;
value rec skip_to_eol =
  parser
  [ [: `'\n' | '\026' | '\012'; s :] -> ()
  | [: `c ; s :] -> skip_to_eol s 
  ]
;
value ident_char =
  parser 
  [ [: `('a'..'z' | 'A'..'Z' | '.' | ':' | '"' | '~' | '\'' | '+' | '-' | '$' as c) :] 
    -> c ]
;
value rec ident buff =
  parser
  [ [: c = ident_char; s :] -> ident (store buff c) s
  | [: :] -> Buffer.contents buff
  ]
;
value next_token_fun =
  let rec next_token buff =
      parser _bp
      [ [: c = ident_char; s = ident (store buff c) :] -> IDENT s
      | [: `('0'..'9' as c); s = number (store buff c) :] -> INT (int_of_string s)
      | [: `c :] _ep -> KEYWORD (String.make 1 c)
      ] in
    let rec next_token_loc =
      parser bp
      [ [: `'%'; _ = skip_to_eol; s :] -> next_token_loc s (* comments skipped *)
      | [: `' ' | '\n' | '\r' | '\t' | '\026' | '\012'; s :] -> next_token_loc s
      | [: tok = next_token (Buffer.create 80) :] ep -> (tok, (bp, ep))
      | [: _ = Stream.empty :] -> (EOI, (bp, succ bp))
      ] in
 next_token_loc
;
value mk () =
 let err loc msg = Loc.raise loc (Token.Error.E msg) in
 fun init_loc cstrm -> Stream.from 
     (fun _ -> try let (tok, (bp, ep)) = next_token_fun cstrm in
                   let loc = Loc.move `start bp (Loc.move `stop ep init_loc) in
                   Some (tok, loc)
               with
               [ Stream.Error str ->
                 let bp = Stream.count cstrm in
                 let loc = Loc.move `start bp (Loc.move `stop (bp+1) init_loc) in
                 err loc str ])
;
end; 
