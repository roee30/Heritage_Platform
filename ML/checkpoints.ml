(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* Checkpoints management *)
open Phases.Phases
  
(* [string_of_phase phase_of_string] *)
(* string encoding of a phase, used to transmit checkpoints in URLs *)
let rec phase_encode =
  function
  | Comp ((ph, ph'), prev, form) ->
      "<{" ^
        ((string_of_phase ph) ^
           ("}{" ^
              ((string_of_phase ph') ^
                 ("}{" ^
                    ((Canon.decode prev) ^
                       ("}{" ^ ((Canon.decode form) ^ "}>")))))))
  | phase -> "{" ^ ((string_of_phase phase) ^ "}")
and bool_encode b = if b then "t" else "f"
  
let string_point (k, (phase, rword), select) =
  let segment = Canon.rdecode rword
  in
    (string_of_int k) ^
      ("," ^
         ((phase_encode phase) ^
            (",{" ^ (segment ^ ("},{" ^ ((bool_encode select) ^ "}"))))))
  
let rec string_points =
  function
  | [] -> ""
  | [ last ] -> string_point last
  | first :: rest -> (string_point first) ^ ("|" ^ (string_points rest))
  
open Bank_lexer
  
module Gram = Camlp4.PreCast.MakeGram(Bank_lexer)
  
open Bank_lexer.Token
  
let cpts = Gram.Entry.mk "cpts"
and lcpt = Gram.Entry.mk "lcpt"
and phase_rword = Gram.Entry.mk "phase_rword"
and cpt = Gram.Entry.mk "cpt"
and phase = Gram.Entry.mk "phase"
and guess_morph = Gram.Entry.mk "guess_morph"
  
(* for interface *)
let _ =
  (Gram.extend (cpts : 'cpts Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Snterm (Gram.Entry.obj (lcpt : 'lcpt Gram.Entry.t)) ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) ->
                      (failwith "Wrong checkpoints parsing\n" : 'cpts))));
               ([ Gram.Snterm (Gram.Entry.obj (lcpt : 'lcpt Gram.Entry.t));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (l : 'lcpt)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (l : 'cpts)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (lcpt : 'lcpt Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0sep
                    ((Gram.Snterm (Gram.Entry.obj (cpt : 'cpt Gram.Entry.t))),
                    (Gram.Skeyword "|")) ],
                (Gram.Action.mk
                   (fun (l : 'cpt list) (_loc : Gram.Loc.t) -> (l : 'lcpt)))) ]) ]))
        ());
   Gram.extend (phase : 'phase Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* Preverbed *)
                Gram.Stoken
                  (((function | TEXT ((_)) -> true | _ -> false), "TEXT _")) ],
                (Gram.Action.mk
                   (fun (p : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      (let p = Gram.Token.extract_string p
                       in phase_of_string p : 'phase))));
               ([ Gram.Skeyword "<";
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Skeyword ">" ],
                (Gram.Action.mk
                   (fun _ (form : Gram.Token.t) (pre : Gram.Token.t)
                      (p' : Gram.Token.t) (p : Gram.Token.t) _
                      (_loc : Gram.Loc.t) ->
                      (let form = Gram.Token.extract_string form in
                       let pre = Gram.Token.extract_string pre in
                       let p' = Gram.Token.extract_string p' in
                       let p = Gram.Token.extract_string p
                       in
                         Comp (((phase_of_string p), (phase_of_string p')),
                           (Encode.code_string pre),
                           (Encode.code_string form)) :
                        'phase)))) ]) ]))
        ());
   Gram.extend (phase_rword : 'phase_rword Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Snterm (Gram.Entry.obj (phase : 'phase Gram.Entry.t));
                  Gram.Skeyword ",";
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _")) ],
                (Gram.Action.mk
                   (fun (o : Gram.Token.t) _ (s : 'phase) (_loc : Gram.Loc.t)
                      ->
                      (let o = Gram.Token.extract_string o
                       in (s, (Encode.rev_code_string o)) : 'phase_rword)))) ]) ]))
        ());
   Gram.extend (cpt : 'cpt Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _"));
                  Gram.Skeyword ",";
                  Gram.Snterm
                    (Gram.Entry.obj (phase_rword : 'phase_rword Gram.Entry.t));
                  Gram.Skeyword ",";
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _")) ],
                (Gram.Action.mk
                   (fun (s : Gram.Token.t) _ (p : 'phase_rword) _
                      (m : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      (let s = Gram.Token.extract_string s in
                       let m = Gram.Token.extract_string m
                       in ((int_of_string m), p, (s = "t")) : 'cpt)))) ]) ]))
        ());
   Gram.extend (guess_morph : 'guess_morph Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Skeyword ",";
                  Gram.Stoken
                    (((function | TEXT ((_)) -> true | _ -> false), "TEXT _"));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (o : Gram.Token.t) _
                      (n : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI ->
                          (let o = Gram.Token.extract_string o in
                           let n = Gram.Token.extract_string n in (n, o) :
                            'guess_morph)
                      | _ -> assert false))) ]) ]))
        ()))
  
let parse_cpts s =
  try Gram.parse_string cpts Loc.ghost s
  with | _ -> raise (Control.Anomaly "parse_cpts")
  
let parse_guess s =
  try Gram.parse_string guess_morph Loc.ghost s
  with | _ -> raise (Control.Anomaly "parse_guess")
  
(* Parsing projections stream (Parser, Regression) *)
let projs = Gram.Entry.mk "projs"
and lproj = Gram.Entry.mk "lproj"
and proj = Gram.Entry.mk "proj"
  
(* A stream of projections is encoded under the form [1,2|2,3|...] *)
let _ =
  (Gram.extend (projs : 'projs Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Snterm (Gram.Entry.obj (lproj : 'lproj Gram.Entry.t)) ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) ->
                      (failwith "Wrong projections parsing\n" : 'projs))));
               ([ Gram.Snterm (Gram.Entry.obj (lproj : 'lproj Gram.Entry.t));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (l : 'lproj)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (l : 'projs)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (lproj : 'lproj Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0sep
                    ((Gram.Snterm
                        (Gram.Entry.obj (proj : 'proj Gram.Entry.t))),
                    (Gram.Skeyword "|")) ],
                (Gram.Action.mk
                   (fun (l : 'proj list) (_loc : Gram.Loc.t) -> (l : 'lproj)))) ]) ]))
        ());
   Gram.extend (proj : 'proj Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _"));
                  Gram.Skeyword ",";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (m : Gram.Token.t) _ (n : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      (let m = Gram.Token.extract_string m in
                       let n = Gram.Token.extract_string n
                       in ((int_of_string n), (int_of_string m)) : 'proj)))) ]) ]))
        ()))
  
let parse_proj s =
  try Gram.parse_string projs Loc.ghost s
  with | _ -> raise (Control.Anomaly "parse_proj")
  

