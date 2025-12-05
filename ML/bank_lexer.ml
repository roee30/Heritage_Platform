(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* A simple lexer recognizing idents formed from ASCII letters and integers 
   and skipping spaces and comments between % and eol. 
   Used by [Parse_tree] and [Reader]. *)
module Bank_lexer =
  struct
    open Camlp4.PreCast
      
    open Format
      
    module Loc = Loc
      
    (* Using the PreCast Loc *)
    module Error =
      struct
        type t = string
        
        exception E of t
          
        let to_string x = x
          
        let print = Format.pp_print_string
          
      end
      
    module Token =
      struct
        module Loc = Loc
          
        type t =
          | KEYWORD of string
          | IDENT of string
          | TEXT of string
          | INT of int
          | INTS of int
          | EOI
        
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
                     Stream.icons p
                       (Stream.slazy (fun _ -> filter is_kwd strm)))
              | (* PB        [if is_kwd s then [: `p; filter is_kwd strm :]
              else failwith ("Undefined token: " ^ s)]      *)
                  Some x ->
                  (Stream.junk __strm;
                   let s = __strm
                   in
                     Stream.icons x (Stream.slazy (fun _ -> filter is_kwd s)))
              | _ -> Stream.sempty
              
            let define_filter _ _ = ()
              
            let keyword_added _ _ _ = ()
              
            let keyword_removed _ _ = ()
              
          end
          
        let to_string =
          function
          | KEYWORD s -> sprintf "KEYWORD %S" s
          | IDENT s -> sprintf "IDENT %S" s
          | TEXT s -> sprintf "TEXT %S" s
          | INT i -> sprintf "INT %d" i
          | INTS i -> sprintf "INTS %d" i
          | EOI -> "EOI"
          
        let print ppf x = pp_print_string ppf (to_string x)
          
        let match_keyword kwd =
          function | KEYWORD kwd' -> kwd' = kwd | _ -> false
          
        let extract_string =
          function
          | INT i -> string_of_int i
          | INTS i -> string_of_int i
          | IDENT s | KEYWORD s | TEXT s -> s
          | EOI -> ""
          
      end
      
    open Token
      
    (* The string buffering machinery - ddr + np *)
    let store buf c = (Buffer.add_char buf c; buf)
      
    let rec base_number len (__strm : _ Stream.t) = number len __strm
    and number buf (__strm : _ Stream.t) =
      match Stream.peek __strm with
      | Some (('0' .. '9' as c)) ->
          (Stream.junk __strm; number (store buf c) __strm)
      | _ -> Buffer.contents buf
      
    let rec skip_to_eol (__strm : _ Stream.t) =
      match Stream.peek __strm with
      | Some ('\n' | '\026' | '\012') -> (Stream.junk __strm; ())
      | Some c -> (Stream.junk __strm; skip_to_eol __strm)
      | _ -> raise Stream.Failure
      
    let ident_char (__strm : _ Stream.t) =
      match Stream.peek __strm with
      | Some (('a' .. 'z' | 'A' .. 'Z' | '.' | ':' | '"' | '~' | '\'' as c))
          -> (Stream.junk __strm; c)
      | _ -> raise Stream.Failure
      
    let rec ident2 buff (__strm : _ Stream.t) =
      match try Some (ident_char __strm) with | Stream.Failure -> None with
      | Some c -> ident2 (store buff c) __strm
      | _ ->
          (match Stream.peek __strm with
           | Some (('0' .. '9' as c)) ->
               (Stream.junk __strm; ident2 (store buff c) __strm)
           | _ -> Buffer.contents buff)
      
    let rec text buff (__strm : _ Stream.t) =
      match Stream.peek __strm with
      | Some '}' -> (Stream.junk __strm; Buffer.contents buff)
      | Some '{' ->
          (Stream.junk __strm;
           let buff =
             (try text_buff (store buff '{') __strm
              with | Stream.Failure -> raise (Stream.Error ""))
           in text (store buff '}') __strm)
      | Some c -> (Stream.junk __strm; text (store buff c) __strm)
      | _ -> raise Stream.Failure
    and text_buff buff (__strm : _ Stream.t) =
      match Stream.peek __strm with
      | Some '}' -> (Stream.junk __strm; buff)
      | Some '{' ->
          (Stream.junk __strm;
           let buff =
             (try text_buff (store buff '{') __strm
              with | Stream.Failure -> raise (Stream.Error ""))
           in text_buff (store buff '}') __strm)
      | Some c -> (Stream.junk __strm; text_buff (store buff c) __strm)
      | _ -> raise Stream.Failure
      
    let next_token_fun =
      let rec next_token buff (__strm : _ Stream.t) =
        let _bp = Stream.count __strm
        in
          match Stream.peek __strm with
          | Some '{' ->
              (Stream.junk __strm;
               let t =
                 (try text buff __strm
                  with | Stream.Failure -> raise (Stream.Error ""))
               in TEXT t)
          | Some (('1' .. '9' as c)) ->
              (Stream.junk __strm;
               let s =
                 (try number (store buff c) __strm
                  with | Stream.Failure -> raise (Stream.Error ""))
               in INT (int_of_string s))
          | Some '0' ->
              (Stream.junk __strm;
               let s =
                 (try base_number (store buff '0') __strm
                  with | Stream.Failure -> raise (Stream.Error ""))
               in INT (int_of_string s))
          | _ ->
              (match try Some (ident_char __strm)
                     with | Stream.Failure -> None
               with
               | Some c ->
                   let s =
                     (try ident2 (store buff c) __strm
                      with | Stream.Failure -> raise (Stream.Error ""))
                   in
                     if s = "Comment"
                     then KEYWORD "Comment"
                     else
                       if s = "Example"
                       then KEYWORD "Example"
                       else
                         if s = "Continue"
                         then KEYWORD "Continue"
                         else
                           if s = "Source"
                           then KEYWORD "Source"
                           else
                             if s = "Parse"
                             then KEYWORD "Parse"
                             else
                               if s = "Gloss"
                               then KEYWORD "Gloss"
                               else IDENT s
               | _ ->
                   (match Stream.peek __strm with
                    | Some c ->
                        (Stream.junk __strm;
                         let _ep = Stream.count __strm
                         in KEYWORD (String.make 1 c))
                    | _ -> raise Stream.Failure)) in
      let rec next_token_loc (__strm : _ Stream.t) =
        let bp = Stream.count __strm
        in
          match Stream.peek __strm with
          | Some '%' ->
              (Stream.junk __strm;
               let _ =
                 (try skip_to_eol __strm
                  with | Stream.Failure -> raise (Stream.Error ""))
               in next_token_loc __strm)
          | Some (' ' | '\n' | '\r' | '\t' | '\026' | '\012') ->
              (Stream.junk __strm; next_token_loc __strm)
          | Some '^' ->
              (Stream.junk __strm;
               let s = __strm in
               let (tok, loc) = next_token_loc s
               in
                 (match tok with
                  | INT n -> ((INTS n), loc)
                  | _ -> raise (Token.Error.E "+n")))
          | (* for Gillon's dislocated phrases *) Some '!' ->
              (Stream.junk __strm;
               let s = __strm in
               let (tok, loc) = next_token_loc s
               in
                 (match tok with
                  | INT n -> ((INTS (- n)), loc)
                  | _ -> raise (Token.Error.E "-n")))
          | _ -> (* for Gillon's dislocation context *)
              (match try Some (next_token (Buffer.create 80) __strm)
                     with | Stream.Failure -> None
               with
               | Some tok -> let ep = Stream.count __strm in (tok, (bp, ep))
               | _ -> let _ = Stream.empty __strm in (EOI, (bp, (succ bp))))
      in next_token_loc
      
    let mk () =
      let err loc msg = Loc.raise loc (Token.Error.E msg)
      in
        fun init_loc cstrm ->
          let lexer _ =
            try
              let (tok, (bp, ep)) = next_token_fun cstrm in
              let loc = Loc.move `start bp (Loc.move `stop ep init_loc)
              in Some ((tok, loc))
            with
            | Stream.Error str ->
                let bp = Stream.count cstrm in
                let loc =
                  Loc.move `start bp (Loc.move `stop (bp + 1) init_loc)
                in err loc str
          in Stream.from lexer
      
  end
  

