(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI utilities *)

(*i module Cgi = struct i*)

(* Decoding utilities, author Daniel de Rauglaudre *)
(* ddr begin *)

value hexa_val conf =
  match conf with
  [ '0'..'9' -> Char.code conf - Char.code '0'
  | 'a'..'f' -> Char.code conf - Char.code 'a' + 10
  | 'A'..'F' -> Char.code conf - Char.code 'A' + 10
  | _ -> 0 
  ]
;
value decode_url s =
  let rec need_decode i =
    if i < Bytes.length s then
      match s.[i] with
      [ '%' | '+' -> True
      | _ -> need_decode (succ i)
      ]
    else False in
  let rec compute_len i i1 =
    if i < Bytes.length s then
      let i =
        match s.[i] with
        [ '%' when i + 2 < Bytes.length s -> i + 3
        | _ -> succ i
        ]
      in
      compute_len i (succ i1)
    else i1 in
  let rec copy_decode_in s1 i i1 =
    if i < Bytes.length s then
      let i =
        match s.[i] with
        [ '%' when i + 2 < Bytes.length s ->
            let v = hexa_val s.[i + 1] * 16 + hexa_val s.[i + 2] 
            in do {Bytes.set s1 i1 (Char.chr v); i + 3}
        | '+' -> do {Bytes.set s1 i1 ' '; succ i}
        | x -> do {Bytes.set s1 i1 x; succ i} 
        ] in
      copy_decode_in s1 i (succ i1)
    else s1 in
  let rec strip_heading_and_trailing_spaces s =
    if Bytes.length s > 0 then
      if s.[0] == ' ' then
        strip_heading_and_trailing_spaces (Bytes.sub s 1 (Bytes.length s - 1))
      else if s.[Bytes.length s - 1] == ' ' then
        strip_heading_and_trailing_spaces (Bytes.sub s 0 (Bytes.length s - 1))
      else s
    else s in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = Bytes.create len in
    strip_heading_and_trailing_spaces (copy_decode_in s1 0 0)
  else s;

(* ça convertit une chaine venant de l'URL en une a-list; la chaine est
   une suite de paires clé=valeur séparées par des ; ou des \& *)

value create_env s =
  let rec get_assoc beg i =
    if i == Bytes.length s then
      if i == beg then [] else [Bytes.sub s beg (i - beg)]
    else if s.[i] == ';' || s.[i] == '&' then
      let next_i = succ i in
      [Bytes.sub s beg (i - beg) :: get_assoc next_i next_i]
    else get_assoc beg (succ i) in
  let rec separate i s =
    if i = Bytes.length s then (s, "")
    else if s.[i] == '=' then
      (Bytes.sub s 0 i, Bytes.sub s (succ i) (Bytes.length s - succ i))
    else separate (succ i) s in
  List.map (separate 0) (get_assoc 0 0)
;

(* ddr end *)

value get key alist default = 
  try List.assoc key alist with [ Not_found -> default ] 
;
value decoded_get key default alist = decode_url (get key alist default)
;
value query_string_env_var = "QUERY_STRING"
;
value query_string () =
  try Sys.getenv query_string_env_var with [ Not_found -> "" ]
;
value url_encode s =
  let hexa_str c = Printf.sprintf "%.2X" (Char.code c) in

  (* Reference: RFC 3986 appendix A *)
  let url_encode = fun
    (* Unreserved characters *)
    [ 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_' | '~' as c ->
      String.make 1 c

    (* Special case of the space character *)
    | ' ' -> "+"

    (* Reserved characters *)
    | c -> "%" ^ hexa_str c
    ]
  in

  let char_of_string s =
    if String.length s = 1 then s.[0] else failwith "char_of_string"
  in
  let subst s = s |> Str.matched_string |> char_of_string |> url_encode in
  let any_char = Str.regexp ".\\|\n" in
  Str.global_substitute any_char subst s
;
value query_of_env env =
  String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ url_encode v) env)
;
value url ?query ?fragment path =
  let opt_part prefix = fun
    [ None -> ""
    | Some part -> prefix ^ part
    ]
  in
  let query_part = opt_part "?" query in
  let fragment_part = opt_part "#" fragment in
  path ^ query_part ^ fragment_part
;
(*i end; i*)
