(**************************************************************************)
(*                                                                        *)
(*                 The Zen Computational Linguistics Toolkit              *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2007 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* A very simple lexer recognizing 1 character idents and integers 
   and skipping spaces and comments between [%] and eol; 
   used for various transduction tasks with Camlp4 Grammars. 
   It is a copy of [ZEN/zen_lexer.ml] in order to simplify dependencies. *)
(*i module Min_lexer = struct i*)
open Camlp4.PreCast
  
open Format
  
module Loc = Loc
  
(* Using the PreCast Loc *)
module Error = Camlp4.Struct.EmptyError
  
(* Dummy Error module *)
module Token =
  struct
    module Loc = Loc
      
    type t = | KEYWORD of string | LETTER of string | INT of int | EOI
    
    module Error = Error
      
    module Filter =
      struct
        type token_filter = (t, Loc.t) Camlp4.Sig.stream_filter
        
        type t = string -> bool
        
        let mk is_kwd = is_kwd
          
        let rec filter is_kwd (__strm : _ Stream.t) =
          match Stream.peek __strm with
          | Some (((KEYWORD s, loc) as p)) ->
              (Stream.junk __strm;
               let strm = __strm
               in
                 if is_kwd s
                 then
                   Stream.icons p
                     (Stream.slazy (fun _ -> filter is_kwd strm))
                 else failwith ("Undefined token: " ^ s))
          | Some x ->
              (Stream.junk __strm;
               let s = __strm
               in Stream.icons x (Stream.slazy (fun _ -> filter is_kwd s)))
          | _ -> Stream.sempty
          
        let define_filter _ _ = ()
          
        let keyword_added _ _ _ = ()
          
        let keyword_removed _ _ = ()
          
      end
      
    let to_string =
      function
      | KEYWORD s -> sprintf "KEYWORD %S" s
      | LETTER s -> sprintf "LETTER %S" s
      | INT i -> sprintf "INT %d" i
      | EOI -> "EOI"
      
    let print ppf x = pp_print_string ppf (to_string x)
      
    let match_keyword kwd =
      function | KEYWORD kwd' when kwd' = kwd -> true | _ -> false
      
    let extract_string =
      function
      | INT i -> string_of_int i
      | LETTER s | KEYWORD s -> s
      | EOI -> ""
      
  end
  
open Token
  
(* The string buffering machinery. *)
(*i ddr + np i*)
let store buf c = (Buffer.add_char buf c; buf)
  
let rec number buf (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some (('0' .. '9' as c)) ->
      (Stream.junk __strm; number (store buf c) __strm)
  | _ -> Buffer.contents buf
  
let rec skip_to_eol (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some ('\n' | '\026' | '\012') -> (Stream.junk __strm; ())
  | Some c -> (Stream.junk __strm; skip_to_eol __strm)
  | _ -> raise Stream.Failure
  
let next_token_fun =
  let rec next_token (__strm : _ Stream.t) =
    let _bp = Stream.count __strm
    in
      match Stream.peek __strm with
      | Some '%' ->
          (Stream.junk __strm;
           let _ =
             (try skip_to_eol __strm
              with | Stream.Failure -> raise (Stream.Error ""))
           in next_token __strm)
      | Some
          (('a' .. 'z' | 'A' .. 'Z' | '\192' .. '\246' | '\248' .. '\255' as
            (*  [| '_'] *) c))
          -> (Stream.junk __strm; LETTER (String.make 1 c))
      | Some (('0' .. '9' as c)) ->
          (Stream.junk __strm;
           let s =
             (try number (store (Buffer.create 80) c) __strm
              with | Stream.Failure -> raise (Stream.Error ""))
           in INT (int_of_string s))
      | Some c ->
          (Stream.junk __strm;
           let _ep = Stream.count __strm in KEYWORD (String.make 1 c))
      | _ -> raise Stream.Failure in
  let rec next_token_loc (__strm : _ Stream.t) =
    let bp = Stream.count __strm
    in
      match Stream.peek __strm with
      | Some (' ' | '\n' | '\r' | '\t' | '\026' | '\012') ->
          (Stream.junk __strm; next_token_loc __strm)
      | _ ->
          (match try Some (next_token __strm) with | Stream.Failure -> None
           with
           | Some tok -> let ep = Stream.count __strm in (tok, (bp, ep))
           | _ -> let _ = Stream.empty __strm in (EOI, (bp, (succ bp))))
  in next_token_loc
  
let mk () init_loc cstrm =
  Stream.from
    (fun _ ->
       let (tok, (bp, ep)) = next_token_fun cstrm in
       let loc = Loc.move `start bp (Loc.move `stop ep init_loc)
       in Some ((tok, loc)))
  

