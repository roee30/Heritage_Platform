(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(*i module Transduction = struct i*)
open Camlp4.PreCast
  
(* MakeGram Loc *)
module Gram = MakeGram(Min_lexer)
  
open Min_lexer.Token
  
let transducer trad t =
  try Gram.parse_string trad Loc.ghost t
  with
  | Loc.Exc_located (loc, e) ->
      (Format.eprintf "In string \"%s\", at location %a:@." t Loc.print loc;
       raise e)
  
(****************************************)
(* Roman with diacritics, TeX encoding  *)
(****************************************)
let tex = Gram.Entry.mk "skt to tex"
and tex_word = Gram.Entry.mk "skt to tex word"
  
let _ =
  (Gram.extend (* skt to tex *) (tex : 'tex Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* deprecated *) (* recommended *) (* patch for latin *)
                (* deprecated *) (* recommended *)
                (* glitch prefix candrabindu *) (* candrabindu *)
                (* candrabindu *) (* candrabindu *) (* hyphenation hint *)
                (* pra-uga *) (* hiatus *) (* reserved *) (* prefix *)
                (* avagraha *) (* ... *) (* homonyms *)
                Gram.Stoken
                  (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i in i : 'tex))));
               ([ Gram.Stoken
                    (((function | LETTER ((_)) -> true | _ -> false),
                      "LETTER _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i in i : 'tex))));
               ([ Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i
                       in "\\(_{" ^ (i ^ "}\\)") : 'tex))));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> ("{\\d D}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "T" -> ("{\\d T}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> ("{\\d h}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> ("{\\d m}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> ("{\\d l}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\""));
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "l", LETTER "l") -> ("{\\LL}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "R" -> ("{\\d R}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> ("{\\d r}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\""));
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "r", LETTER "r") -> ("{\\RR}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> ("{\\d n}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> ("{\\d S}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> ("{\\d s}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> ("{\\d d}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> ("{\\d t}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "."; Gram.Skeyword "."; Gram.Skeyword "." ],
                (Gram.Action.mk
                   (fun _ _ _ (_loc : Gram.Loc.t) -> ("..." : 'tex))));
               ([ Gram.Skeyword "'" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ("'" : 'tex))));
               ([ Gram.Skeyword "-" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ("-" : 'tex))));
               ([ Gram.Skeyword "&" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ("\\&" : 'tex))));
               ([ Gram.Skeyword "_" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ("\\_" : 'tex))));
               ([ Gram.Skeyword "$" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ("\\_" : 'tex))));
               ([ Gram.Skeyword "+" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ("\\-" : 'tex))));
               ([ Gram.Stoken
                    (((function | LETTER "v" -> true | _ -> false),
                      "LETTER \"v\""));
                  Gram.Skeyword "~"; Gram.Skeyword "~" ],
                (Gram.Action.mk
                   (fun _ _ (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t)
                      ->
                      match __camlp4_0 with
                      | LETTER "v" -> ("\\~v" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "y" -> true | _ -> false),
                      "LETTER \"y\""));
                  Gram.Skeyword "~"; Gram.Skeyword "~" ],
                (Gram.Action.mk
                   (fun _ _ (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t)
                      ->
                      match __camlp4_0 with
                      | LETTER "y" -> ("\\~y" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\""));
                  Gram.Skeyword "~"; Gram.Skeyword "~" ],
                (Gram.Action.mk
                   (fun _ _ (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t)
                      ->
                      match __camlp4_0 with
                      | LETTER "l" -> ("\\~l" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "~"; Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\""));
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "l", LETTER "l") -> ("\\~ll" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> ("\\~n" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "U" -> ("U" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\""));
                  Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "U", LETTER "U") -> ("\\=U" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> ("u" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "u") -> ("\\=u" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "I" -> ("I" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\""));
                  Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "I", LETTER "I") -> ("\\=I" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> ("i" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "i") -> ("{\\=\\i}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "A" -> ("A" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\""));
                  Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "A", LETTER "A") -> ("\\=A" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> ("a" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "a", LETTER "a") -> ("\\=a" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "a", LETTER "a", LETTER "a") ->
                          (failwith "Unexpected phantom phoneme" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "a", LETTER "a") ->
                          (failwith "Unexpected phantom phoneme" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "a", LETTER "a") ->
                          (failwith "Unexpected phantom phoneme" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> ("\\'o" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> ("\\'e" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> ("\\'u" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> ("{\\'\\i}" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> ("\\'a" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "Z" -> true | _ -> false),
                      "LETTER \"Z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Z" -> ("\\'S" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> ("\\'S" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> ("\\'s" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> ("\\'s" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "F" -> true | _ -> false),
                      "LETTER \"F\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "F" -> ("f" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "f" -> true | _ -> false),
                      "LETTER \"f\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "f" -> ("\\.n" : 'tex)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> ("\\.n" : 'tex)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (tex_word : 'tex_word Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm (Gram.Entry.obj (tex : 'tex Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'tex list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (String.concat "" w : 'tex_word)
                      | _ -> assert false))) ]) ]))
        ()))
  
let skt_to_tex = transducer tex_word
  
(*******************************************************************)
(* Roman with diacritics, HTML decimal encoding for Unicode points *)
(*******************************************************************)
let html_code = Gram.Entry.mk "skt to html code"
and html = Gram.Entry.mk "skt to html"
  
let _ =
  (Gram.extend (* skt to HTML string *) (html_code : 'html_code Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* patch for latin *) (* candrabindu as m with dot above *)
                (* we lose accents *) (* glitch prefix candrabindu *)
                (* candrabindu  *) (* "\&#173;" = \&shy; cesure prints - *)
                (* pra-uga *) (* hiatus *) (* prefix *) (* reserved *)
                (* avagraha *) (* ... *) (* homonymy index *)
                (* phantom phoneme *a *) (* phantom phoneme *i *)
                (* phantom phoneme *u *) (* phantom phoneme *r *)
                (* phantom phoneme *C *) (* amuissement *)
                Gram.Stoken
                  (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i in i : 'html_code))));
               ([ Gram.Stoken
                    (((function | LETTER ((_)) -> true | _ -> false),
                      "LETTER _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i in i : 'html_code))));
               ([ Gram.Skeyword "["; Gram.Skeyword "-"; Gram.Skeyword "]" ],
                (Gram.Action.mk
                   (fun _ _ _ (_loc : Gram.Loc.t) -> ("[-]" : 'html_code))));
               ([ Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "C" -> true | _ -> false),
                      "LETTER \"C\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "C" -> ("|ch" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> ("|&#7771;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> ("|u" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> ("|i" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> ("|a" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i in "_" ^ i :
                        'html_code))));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> ("&#7692;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "T" -> ("&#7788;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> ("&#7717;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> ("&#7747;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> ("&#7735;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\""));
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "l", LETTER "l") -> ("&#7737;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "R" -> ("&#7770;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> ("&#7771;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\""));
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "r", LETTER "r") -> ("&#7773;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> ("&#7751;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> ("&#7778;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> ("&#7779;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> ("&#7693;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> ("&#7789;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "."; Gram.Skeyword "."; Gram.Skeyword "." ],
                (Gram.Action.mk
                   (fun _ _ _ (_loc : Gram.Loc.t) -> ("..." : 'html_code))));
               ([ Gram.Skeyword "'" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("'" : 'html_code))));
               ([ Gram.Skeyword "&" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("&amp;" : 'html_code))));
               ([ Gram.Skeyword "-" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("-" : 'html_code))));
               ([ Gram.Skeyword "_" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("_" : 'html_code))));
               ([ Gram.Skeyword "$" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("_" : 'html_code))));
               ([ Gram.Skeyword "+" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("" : 'html_code))));
               ([ Gram.Skeyword "~"; Gram.Skeyword "~" ],
                (Gram.Action.mk
                   (fun _ _ (_loc : Gram.Loc.t) -> ("&#7745;" : 'html_code))));
               ([ Gram.Skeyword "~"; Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\""));
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "l", LETTER "l") ->
                          ("l&#7745;l" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> ("&#241;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "U" -> ("U" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\""));
                  Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "U", LETTER "U") -> ("&#362;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> ("u" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "u") -> ("&#363;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "I" -> ("I" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\""));
                  Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "I", LETTER "I") -> ("&#298;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> ("i" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "i") -> ("&#299;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "A" -> ("A" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\""));
                  Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "A", LETTER "A") -> ("&#256;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> ("a" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "a", LETTER "a") -> ("&#257;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> ("o" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> ("e" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> ("u" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> ("i" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> ("a" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> ("&#7745;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "Z" -> true | _ -> false),
                      "LETTER \"Z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Z" -> ("&#346;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> ("&#346;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> ("&#347;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> ("&#347;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "F" -> true | _ -> false),
                      "LETTER \"F\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "F" -> ("f" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "f" -> true | _ -> false),
                      "LETTER \"f\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "f" -> ("&#7749;" : 'html_code)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> ("&#7749;" : 'html_code)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (html : 'html Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm
                       (Gram.Entry.obj (html_code : 'html_code Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'html_code list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (String.concat "" w : 'html)
                      | _ -> assert false))) ]) ]))
        ()))
  
let skt_to_html = transducer html
  
(*******************************)
(* Inverse to [Cgi.decode_url] *)
(*******************************)
let url_letter = Gram.Entry.mk "skt to url letter"
and url = Gram.Entry.mk "skt to url"
  
(* Important: accents and avagraha are removed from the input stream *)
(* Should be isomorphic to [code_rawu] *)
let _ =
  (Gram.extend (* skt to url *) (url_letter : 'url_letter Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* accents and avagraha hidden *)
                (*    | "'" {\R} "%27" (* if preserved *) *) (* "%2B" *)
                (*i   | "&" {\R} "%26" (* unused *)
      | "%" {\R} "%25"
      | "#" {\R} "%23"
      | "@" {\R} "%40"
      | ":" {\R} "%3A"
      | ";" {\R} "%3B"
      | "<" {\R} "%3C"
      | "|" {\R} "%7C"
      | "=" {\R} "%3D"
      | ">" {\R} "%3E"
      | "?" {\R} "%3F"
      | "/" {\R} "%2F"
      | "," {\R} "%2C" i*)
                Gram.Stoken
                  (((function | LETTER ((_)) -> true | _ -> false),
                    "LETTER _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i in i :
                        'url_letter))));
               ([ Gram.Skeyword "$" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("%24" : 'url_letter))));
               ([ Gram.Skeyword "_" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("_" : 'url_letter))));
               ([ Gram.Skeyword " " ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("+" : 'url_letter))));
               ([ Gram.Skeyword "-" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("-" : 'url_letter))));
               ([ Gram.Skeyword "+" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("" : 'url_letter))));
               ([ Gram.Skeyword "." ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("." : 'url_letter))));
               ([ Gram.Skeyword "'" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("" : 'url_letter))));
               ([ Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i in "%23" ^ i :
                        'url_letter))));
               ([ Gram.Skeyword "~" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("%7E" : 'url_letter))));
               ([ Gram.Skeyword "\"" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("%22" : 'url_letter)))) ]) ]))
        ());
   Gram.extend (url : 'url Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm
                       (Gram.Entry.obj
                          (url_letter : 'url_letter Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'url_letter list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (String.concat "" w : 'url)
                      | _ -> assert false))) ]) ]))
        ()))
  
let encode_url = transducer url
  
(*************************************************)
(* Devanagari in Velthuis devnag transliteration *)
(*************************************************)
let dev = Gram.Entry.mk "dev symbol"
and dev_word = Gram.Entry.mk "dev word"
  
let _ =
  (Gram.extend (* skt to devnag *) (dev : 'dev Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* candrabindu *) (* glitch prefix candrabindu *)
                (* candrabindu *) (* hiatus *) (* "{}" in devnag 1.6 *)
                (* suffix *) (* avagraha *) (* homo index ignored *)
                Gram.Stoken
                  (((function | LETTER ((_)) -> true | _ -> false),
                    "LETTER _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i in i : 'dev))));
               ([ Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false),
                      "INT ((_))")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | INT ((_)) -> ("" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> (".h" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (".m" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> (".l" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> (".r" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\""));
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "r", LETTER "r") -> (".R" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (".n" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (".s" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> (".d" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> (".t" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (".a" : 'dev))));
               ([ Gram.Skeyword "-" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ("@" : 'dev))));
               ([ Gram.Skeyword "$" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ("$$" : 'dev))));
               ([ Gram.Skeyword "+" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ("" : 'dev))));
               ([ Gram.Skeyword "~"; Gram.Skeyword "~" ],
                (Gram.Action.mk (fun _ _ (_loc : Gram.Loc.t) -> ("/" : 'dev))));
               ([ Gram.Skeyword "~"; Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\""));
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "l", LETTER "l") -> ("\\~ll" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> ("~n" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> ("u" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "u") -> ("uu" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> ("i" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "i") -> ("ii" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> ("a" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "a", LETTER "a") -> ("aa" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> ("o" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> ("e" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> ("u" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> ("i" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> ("a" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> ("\"s" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> ("\"s" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> ("/" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "f" -> true | _ -> false),
                      "LETTER \"f\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "f" -> ("\"n" : 'dev)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> ("\"n" : 'dev)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (dev_word : 'dev_word Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm (Gram.Entry.obj (dev : 'dev Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'dev list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (String.concat "" w : 'dev_word)
                      | _ -> assert false))) ]) ]))
        ()))
  
let skt_to_devnag = transducer dev_word
  
(****************************************)
(* Greek and math symbols, TeX encoding *)
(****************************************)
let texmath = Gram.Entry.mk "math in tex"
and texmath_word = Gram.Entry.mk "math in tex word"
  
let _ =
  (Gram.extend (* Greek and Math to TeX *) (texmath : 'texmath Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* degree *) (*i   | "^" -> "^" i*)
                Gram.Stoken
                  (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i in i : 'texmath))));
               ([ Gram.Skeyword "," ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> (", " : 'texmath))));
               ([ Gram.Skeyword "=" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("=" : 'texmath))));
               ([ Gram.Skeyword "~" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("{\\sim}" : 'texmath))));
               ([ Gram.Skeyword "!" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("\\!" : 'texmath))));
               ([ Gram.Skeyword "|" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("{\\mid}" : 'texmath))));
               ([ Gram.Skeyword "'" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("'" : 'texmath))));
               ([ Gram.Skeyword "@" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("{}^{\\circ}" : 'texmath))));
               ([ Gram.Skeyword "+" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("+" : 'texmath))));
               ([ Gram.Skeyword "*" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("{\\times}" : 'texmath))));
               ([ Gram.Stoken
                    (((function | LETTER "Z" -> true | _ -> false),
                      "LETTER \"Z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Z" -> ("{\\rm Z}" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> ("\\zeta" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "y" -> true | _ -> false),
                      "LETTER \"y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "y" -> ("\\eta" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "X" -> true | _ -> false),
                      "LETTER \"X\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "X" -> ("\\Xi" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "x" -> true | _ -> false),
                      "LETTER \"x\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "x" -> ("\\xi" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "W" -> true | _ -> false),
                      "LETTER \"W\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "W" -> ("\\Omega" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "w" -> true | _ -> false),
                      "LETTER \"w\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "w" -> ("\\omega" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "v" -> true | _ -> false),
                      "LETTER \"v\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "v" -> ("\\varsigma" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "U" -> ("\\Upsilon" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> ("\\upsilon" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> ("\\tau" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> ("\\Sigma" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> ("\\sigma" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> ("\\rho" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "q" -> true | _ -> false),
                      "LETTER \"q\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "q" -> ("\\chi" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "P" -> true | _ -> false),
                      "LETTER \"P\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "P" -> ("\\Pi" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "p" -> ("\\pi" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "O" -> true | _ -> false),
                      "LETTER \"O\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "O" -> ("{\\rm O}" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> (" o" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> ("\\nu" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> ("\\mu" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "L" -> true | _ -> false),
                      "LETTER \"L\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "L" -> ("\\Lambda" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> ("\\lambda" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "K" -> true | _ -> false),
                      "LETTER \"K\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "K" -> ("{\\rm K}" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "k" -> ("\\kappa" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> ("\\iota" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "H" -> true | _ -> false),
                      "LETTER \"H\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "H" -> ("\\Theta" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> ("\\theta" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "G" -> true | _ -> false),
                      "LETTER \"G\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "G" -> ("\\Psi" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "g" -> ("\\psi" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "F" -> true | _ -> false),
                      "LETTER \"F\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "F" -> ("\\Phi" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "f" -> true | _ -> false),
                      "LETTER \"f\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "f" -> ("\\phi" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> ("\\epsilon" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> ("\\Delta" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> ("\\delta" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "C" -> true | _ -> false),
                      "LETTER \"C\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "C" -> ("\\Gamma" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "c" -> ("\\gamma" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "b" -> ("\\beta" : 'texmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> ("\\alpha" : 'texmath)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (texmath_word : 'texmath_word Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm
                       (Gram.Entry.obj (texmath : 'texmath Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'texmath list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (String.concat "" w : 'texmath_word)
                      | _ -> assert false))) ]) ]))
        ()))
  
let math_to_tex = transducer texmath_word
  
(*****************************************)
(* Greek and math symbols, HTML encoding *)
(*****************************************)
let htmlmath = Gram.Entry.mk "math in html"
and htmlmath_word = Gram.Entry.mk "math in html word"
  
let _ =
  (Gram.extend (* greek and math to html *)
     (htmlmath : 'htmlmath Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* "\&alpha;"   *) (* "\&beta;"    *) (* "\&gamma;"   *)
                (* "\&Gamma;"   *) (* "\&delta;"   *) (* "\&Delta;"   *)
                (* "\&epsilon;" *) (* "\&phi;"     *) (* "\&Phi;"     *)
                (* "\&psi;"     *) (* "\&Psi;"     *) (* "\&theta;"   *)
                (* "\&Theta;"   *) (* "\&iota;"    *) (* "\&kappa;"   *)
                (* "\&Kappa;"   *) (* "\&lambda;"  *) (* "\&Lambda;"  *)
                (* "\&mu;"      *) (* "\&nu;"      *) (* "\&omicron;" *)
                (* "\&Omicron;" *) (* "\&pi;"      *) (* "\&Pi;"      *)
                (* "\&chi;"     *) (* "\&rho;"     *) (* "\&sigma;"   *)
                (* "\&Sigma;"   *) (* "\&tau;"     *) (* "\&upsilon;" *)
                (* "\&Upsilon;" *) (* "\&sigmaf"   *) (* "\&omega;"   *)
                (* "\&Omega;"   *) (* "\&xi;"      *) (* "\&Xi;"      *)
                (* "\&eta;"     *) (* "\&zeta;"    *) (* "\&Zeta;"    *)
                (* "\&times;"   *) (* "\&deg;"     *) (* "\&prime;"   *)
                (*i   | "^" {\R} "^" i*)
                Gram.Stoken
                  (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i in i : 'htmlmath))));
               ([ Gram.Skeyword "," ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> (", " : 'htmlmath))));
               ([ Gram.Skeyword "=" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("=" : 'htmlmath))));
               ([ Gram.Skeyword "~" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("~" : 'htmlmath))));
               ([ Gram.Skeyword "!" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("!" : 'htmlmath))));
               ([ Gram.Skeyword "|" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("|" : 'htmlmath))));
               ([ Gram.Skeyword "'" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("&#8242;" : 'htmlmath))));
               ([ Gram.Skeyword "@" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("&#176;" : 'htmlmath))));
               ([ Gram.Skeyword "+" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("+" : 'htmlmath))));
               ([ Gram.Skeyword "*" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ("&#215;" : 'htmlmath))));
               ([ Gram.Stoken
                    (((function | LETTER "Z" -> true | _ -> false),
                      "LETTER \"Z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Z" -> ("&#918;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> ("&#950;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "y" -> true | _ -> false),
                      "LETTER \"y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "y" -> ("&#951;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "X" -> true | _ -> false),
                      "LETTER \"X\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "X" -> ("&#926;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "x" -> true | _ -> false),
                      "LETTER \"x\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "x" -> ("&#958;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "W" -> true | _ -> false),
                      "LETTER \"W\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "W" -> ("&#937;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "w" -> true | _ -> false),
                      "LETTER \"w\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "w" -> ("&#969;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "v" -> true | _ -> false),
                      "LETTER \"v\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "v" -> ("&#962;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "U" -> ("&#933;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> ("&#965;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> ("&#964;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> ("&#931;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> ("&#963;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> ("&#961;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "q" -> true | _ -> false),
                      "LETTER \"q\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "q" -> ("&#967;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "P" -> true | _ -> false),
                      "LETTER \"P\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "P" -> ("&#960;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "p" -> ("&#960;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "O" -> true | _ -> false),
                      "LETTER \"O\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "O" -> ("&#927;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> ("&#959;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> ("&#957;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> ("&#956;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "L" -> true | _ -> false),
                      "LETTER \"L\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "L" -> ("&#923;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> ("&#955;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "K" -> true | _ -> false),
                      "LETTER \"K\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "K" -> ("&#922;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "k" -> ("&#954;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> ("&#953;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "H" -> true | _ -> false),
                      "LETTER \"H\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "H" -> ("&#920;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> ("&#952;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "G" -> true | _ -> false),
                      "LETTER \"G\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "G" -> ("&#936;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "g" -> ("&#968;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "F" -> true | _ -> false),
                      "LETTER \"F\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "F" -> ("&#934;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "f" -> true | _ -> false),
                      "LETTER \"f\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "f" -> ("&#966;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> ("&#949;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> ("&#916;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> ("&#948;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "C" -> true | _ -> false),
                      "LETTER \"C\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "C" -> ("&#915;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "c" -> ("&#947;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "b" -> ("&#946;" : 'htmlmath)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> ("&#945;" : 'htmlmath)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (htmlmath_word : 'htmlmath_word Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm
                       (Gram.Entry.obj (htmlmath : 'htmlmath Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'htmlmath list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (String.concat "" w : 'htmlmath_word)
                      | _ -> assert false))) ]) ]))
        ()))
  
let math_to_html = transducer htmlmath_word
  
(**********************************************************************)
(* Numeric code encoding, for devanagari sorting and other processing *)
(**********************************************************************)
let lower = Gram.Entry.mk "lower case as letter VH"
and word = Gram.Entry.mk "word VH"
and wx = Gram.Entry.mk "letter WX"
and wordwx = Gram.Entry.mk "word WX"
and kh = Gram.Entry.mk "letter KH"
and wordkh = Gram.Entry.mk "word KH"
and sl = Gram.Entry.mk "letter SL"
and wordsl = Gram.Entry.mk "word SL"
  
(* glitch for allowing prefix candrabindu *)
let candrabindu_fix w =
  let rec candrec acc =
    function
    | [] -> List.rev acc
    | 150 :: r -> candrec (44 :: 15 :: 44 :: acc) r
    | n :: r -> candrec (n :: acc) r
  in candrec [] w
  
let _ =
  (Gram.extend (* skt to nat *) (lower : 'lower Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* removes accents, keeps initial quote as avagraha *)
                  (* compat Velthuis *) (* ziva *) (* compat Velthuis *)
                  (* compat KH *) (* inconsistent with upper *)
                  (* compat Velthuis *) (* glitch prefix candrabindu *)
                  (* candrabindu *)
                  (* OBS | "+"; c=lower -> c (* prevent hyphenation in TeX *) *)
                  (* notation for affixing *) (* notation for compounding *)
                  (* \& = alternate avagraha preserved - legacy *)
                  (* sentential hiatus *)
                  (* accented vowels - accent is lost *) (* pr'a-uga *)
                  (* g'o-agra *) (* avagraha *)
                  (* alternate notation for vigraha *) (* *a *) (* *i *)
                  (* *u *) (* *a *) (* *i *) (* *u *) (* *r *) (* *C *)
                  (* pra-ucya *) (* go-agraa *)
                  (* alternate v rather than raising Stream error *)
                  (* homo *) Gram.Skeyword "["; Gram.Skeyword "-";
                  Gram.Skeyword "]" ],
                (Gram.Action.mk
                   (fun _ _ _ (_loc : Gram.Loc.t) -> ((-2) : 'lower))));
               ([ Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i
                       in 50 + (int_of_string i) : 'lower))));
               ([ Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> (49 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (48 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "w" -> true | _ -> false),
                      "LETTER \"w\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "w" -> (45 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "v" -> true | _ -> false),
                      "LETTER \"v\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "v" -> (45 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> (44 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> (43 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "y" -> true | _ -> false),
                      "LETTER \"y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "y" -> (42 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (41 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (36 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "b" -> (39 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "b") -> (40 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "p" -> (37 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "p") -> (38 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> (34 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "d") -> (35 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> (32 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "t") -> (33 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "j" -> true | _ -> false),
                      "LETTER \"j\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "j" -> (24 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "j" -> true | _ -> false),
                      "LETTER \"j\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "j") -> (25 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "c" -> (22 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "c") -> (23 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "g" -> (19 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "g") -> (20 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "k" -> (17 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "k") -> (18 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> (12 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\""));
                  Gram.Skeyword "$" ],
                (Gram.Action.mk
                   (fun _ (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> (12 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> (10 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> (5 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "u") -> (6 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> (3 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "i") -> (4 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> (1 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "$" ],
                (Gram.Action.mk
                   (fun _ (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> (1 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "a") -> (13 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "a") -> (11 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "a", LETTER "a") -> (2 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "C" -> true | _ -> false),
                      "LETTER \"C\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "C", LETTER "a", LETTER "a") ->
                          (123 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "r", LETTER "a", LETTER "a") ->
                          ((-6) : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "U", LETTER "a", LETTER "a") ->
                          ((-8) : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "I", LETTER "a", LETTER "a") ->
                          ((-7) : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "A", LETTER "a", LETTER "a") ->
                          ((-9) : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "a", LETTER "a") ->
                          ((-5) : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "a", LETTER "a") ->
                          ((-4) : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "|";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t) _
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "a", LETTER "a", LETTER "a") ->
                          ((-3) : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ":" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (16 : 'lower))));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> (16 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (14 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> (9 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> (7 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\""));
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "r", LETTER "r") -> (8 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (31 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (47 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> (29 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "d") -> (30 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> (27 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "t") -> (28 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "."; Gram.Skeyword "."; Gram.Skeyword ".";
                  Gram.Sself ],
                (Gram.Action.mk
                   (fun (c : 'lower) _ _ _ (_loc : Gram.Loc.t) ->
                      (c : 'lower))));
               ([ Gram.Skeyword "'" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ((-1) : 'lower))));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> (12 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\""));
                  Gram.Skeyword "$" ],
                (Gram.Action.mk
                   (fun _ (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t)
                      ->
                      match __camlp4_0 with
                      | LETTER "o" -> (12 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> (10 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> (5 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> (3 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> (1 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Skeyword "$" ],
                (Gram.Action.mk
                   (fun _ (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t)
                      ->
                      match __camlp4_0 with
                      | LETTER "a" -> (1 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "a") -> (13 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "a") -> (11 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "'";
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "a", LETTER "a") -> (2 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "_" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (50 : 'lower))));
               ([ Gram.Skeyword "&" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> ((-1) : 'lower))));
               ([ Gram.Skeyword "+" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (100 : 'lower))));
               ([ Gram.Skeyword "-" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (0 : 'lower))));
               ([ Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (26 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "~"; Gram.Skeyword "~" ],
                (Gram.Action.mk
                   (fun _ _ (_loc : Gram.Loc.t) -> (15 : 'lower))));
               ([ Gram.Skeyword "~"; Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\""));
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "l", LETTER "l") -> (150 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (15 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> (47 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "R" -> (7 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "H" -> true | _ -> false),
                      "LETTER \"H\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "H" -> (16 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "M" -> true | _ -> false),
                      "LETTER \"M\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "M" -> (14 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "G" -> true | _ -> false),
                      "LETTER \"G\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "G" -> (21 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (46 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> (46 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (21 : 'lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "f" -> true | _ -> false),
                      "LETTER \"f\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "f" -> (21 : 'lower)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (* amuissement *)
     (*   Special codes 
     code 50 hiatus Canon.decode [50] = "_" 
     codes 51 to 59 - 9 homonymy indexes 
     code -1 -> "'" (* avagraha *)
     code -2 -> "[-]" (* amuissement *)
     code -3 -> "aa|a" (* sandhi of aa and a *)
     code -4 -> "aa|i" (* sandhi of aa and i *)
     code -5 -> "aa|u" (* sandhi of aa and u *)
     code -6 -> "aa|r" (* sandhi of aa and .r *)
     code -7 -> "aa|I" (* sandhi of aa and ii *)
     code -8 -> "aa|U" (* sandhi of aa and uu *)
     code -9 -> "aa|A" (* sandhi of aa and aa *)
     code 123 -> "aa|C" (* sandhi of aa and ch *)
     codes 101 to 149 reserved for upper case encodings in [Canon.decode_ref]
     codes 124, 149, 249 used for variants resp. j' of j 24 and h',h'' of h 49 
           in [Int_sandhi]  *)
     (word : 'word Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm
                       (Gram.Entry.obj (lower : 'lower Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'lower list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (candrabindu_fix w : 'word)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (wx : 'wx Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* candrabindu *) (* notation for affixing *)
                  (* notation for compounding *) (* sentential hiatus *)
                  (* avagraha *) Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i
                       in 50 + (int_of_string i) : 'wx))));
               ([ Gram.Stoken
                    (((function | LETTER "Z" -> true | _ -> false),
                      "LETTER \"Z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Z" -> ((-1) : 'wx)
                      | _ -> assert false)));
               ([ Gram.Skeyword "_" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (50 : 'wx))));
               ([ Gram.Skeyword "+" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (100 : 'wx))));
               ([ Gram.Skeyword "-" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (0 : 'wx))));
               ([ Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> (49 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (48 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "R" -> (47 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> (46 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "v" -> true | _ -> false),
                      "LETTER \"v\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "v" -> (45 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> (44 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> (43 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "y" -> true | _ -> false),
                      "LETTER \"y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "y" -> (42 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (41 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "B" -> true | _ -> false),
                      "LETTER \"B\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "B" -> (40 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "b" -> (39 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "P" -> true | _ -> false),
                      "LETTER \"P\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "P" -> (38 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "p" -> (37 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (36 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "X" -> true | _ -> false),
                      "LETTER \"X\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "X" -> (35 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "x" -> true | _ -> false),
                      "LETTER \"x\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "x" -> (34 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "W" -> true | _ -> false),
                      "LETTER \"W\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "W" -> (33 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "w" -> true | _ -> false),
                      "LETTER \"w\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "w" -> (32 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "N" -> true | _ -> false),
                      "LETTER \"N\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "N" -> (31 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> (30 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> (29 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "T" -> (28 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> (27 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "F" -> true | _ -> false),
                      "LETTER \"F\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "F" -> (26 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "J" -> true | _ -> false),
                      "LETTER \"J\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "J" -> (25 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "j" -> true | _ -> false),
                      "LETTER \"j\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "j" -> (24 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "C" -> true | _ -> false),
                      "LETTER \"C\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "C" -> (23 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "c" -> (22 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "f" -> true | _ -> false),
                      "LETTER \"f\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "f" -> (21 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "G" -> true | _ -> false),
                      "LETTER \"G\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "G" -> (20 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "g" -> (19 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "K" -> true | _ -> false),
                      "LETTER \"K\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "K" -> (18 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "k" -> (17 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "H" -> true | _ -> false),
                      "LETTER \"H\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "H" -> (16 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> (15 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "M" -> true | _ -> false),
                      "LETTER \"M\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "M" -> (14 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "O" -> true | _ -> false),
                      "LETTER \"O\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "O" -> (13 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> (12 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "E" -> true | _ -> false),
                      "LETTER \"E\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "E" -> (11 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> (10 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "L" -> true | _ -> false),
                      "LETTER \"L\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "L" -> (9 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "Q" -> true | _ -> false),
                      "LETTER \"Q\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Q" -> (8 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "q" -> true | _ -> false),
                      "LETTER \"q\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "q" -> (7 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "U" -> (6 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> (5 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "I" -> (4 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> (3 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "A" -> (2 : 'wx)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> (1 : 'wx)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (* homo *) (wordwx : 'wordwx Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm (Gram.Entry.obj (wx : 'wx Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'wx list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (w : 'wordwx)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (kh : 'kh Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* candrabindu absent *) (* avagraha *)
                  (* notation for affixing *) (* notation for compounding *)
                  (* sentential hiatus *) (* avagraha missing *)
                  Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i
                       in 50 + (int_of_string i) : 'kh))));
               ([ Gram.Skeyword "_" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (50 : 'kh))));
               ([ Gram.Skeyword "+" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (100 : 'kh))));
               ([ Gram.Skeyword "-" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (0 : 'kh))));
               ([ Gram.Skeyword "'" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ((-1) : 'kh))));
               ([ Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> (49 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (48 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> (47 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> (46 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "v" -> true | _ -> false),
                      "LETTER \"v\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "v" -> (45 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> (44 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> (43 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "y" -> true | _ -> false),
                      "LETTER \"y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "y" -> (42 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (41 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "b" -> (39 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "b") -> (40 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "p" -> (37 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "p") -> (38 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (36 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> (34 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "d") -> (35 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> (32 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "t") -> (33 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "N" -> true | _ -> false),
                      "LETTER \"N\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "N" -> (31 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> (29 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "D") -> (30 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "T" -> (27 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "T") -> (28 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "J" -> true | _ -> false),
                      "LETTER \"J\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "J" -> (26 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "j" -> true | _ -> false),
                      "LETTER \"j\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "j" -> (24 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "j" -> true | _ -> false),
                      "LETTER \"j\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "j") -> (25 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "c" -> (22 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "c") -> (23 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "G" -> true | _ -> false),
                      "LETTER \"G\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "G" -> (21 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "g" -> (19 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "g") -> (20 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "k" -> (17 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "k") -> (18 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "H" -> true | _ -> false),
                      "LETTER \"H\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "H" -> (16 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "M" -> true | _ -> false),
                      "LETTER \"M\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "M" -> (14 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> (1 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "a") -> (13 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> (12 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "a") -> (11 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> (10 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "L" -> true | _ -> false),
                      "LETTER \"L\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "L" -> (9 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "R" -> (7 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\""));
                  Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "R", LETTER "R") -> (8 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "U" -> (6 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> (5 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "I" -> (4 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> (3 : 'kh)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "A" -> (2 : 'kh)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (* homo *) (wordkh : 'wordkh Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm (Gram.Entry.obj (kh : 'kh Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'kh list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (w : 'wordkh)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (sl : 'sl Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* avagraha *) (* notation for affixing *)
                  (* notation for compounding *) (* sentential hiatus *)
                  Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i
                       in 50 + (int_of_string i) : 'sl))));
               ([ Gram.Skeyword "_" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (50 : 'sl))));
               ([ Gram.Skeyword "+" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (100 : 'sl))));
               ([ Gram.Skeyword "-" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (0 : 'sl))));
               ([ Gram.Skeyword "'" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> ((-1) : 'sl))));
               ([ Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> (49 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (48 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> (47 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> (46 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "v" -> true | _ -> false),
                      "LETTER \"v\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "v" -> (45 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> (44 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> (43 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "y" -> true | _ -> false),
                      "LETTER \"y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "y" -> (42 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (41 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "B" -> true | _ -> false),
                      "LETTER \"B\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "B" -> (40 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "b" -> (39 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "P" -> true | _ -> false),
                      "LETTER \"P\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "P" -> (38 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "p" -> (37 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (36 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> (35 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> (34 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "T" -> (33 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> (32 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "R" -> (31 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "Q" -> true | _ -> false),
                      "LETTER \"Q\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Q" -> (30 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "q" -> true | _ -> false),
                      "LETTER \"q\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "q" -> (29 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "W" -> true | _ -> false),
                      "LETTER \"W\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "W" -> (28 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "w" -> true | _ -> false),
                      "LETTER \"w\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "w" -> (27 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "Y" -> true | _ -> false),
                      "LETTER \"Y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Y" -> (26 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "J" -> true | _ -> false),
                      "LETTER \"J\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "J" -> (25 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "j" -> true | _ -> false),
                      "LETTER \"j\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "j" -> (24 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "C" -> true | _ -> false),
                      "LETTER \"C\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "C" -> (23 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "c" -> (22 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "N" -> true | _ -> false),
                      "LETTER \"N\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "N" -> (21 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "G" -> true | _ -> false),
                      "LETTER \"G\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "G" -> (20 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "g" -> (19 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "K" -> true | _ -> false),
                      "LETTER \"K\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "K" -> (18 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "k" -> (17 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "H" -> true | _ -> false),
                      "LETTER \"H\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "H" -> (16 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Skeyword "~" ],
                (Gram.Action.mk (fun _ (_loc : Gram.Loc.t) -> (15 : 'sl))));
               ([ Gram.Stoken
                    (((function | LETTER "M" -> true | _ -> false),
                      "LETTER \"M\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "M" -> (14 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "O" -> true | _ -> false),
                      "LETTER \"O\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "O" -> (13 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> (12 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "E" -> true | _ -> false),
                      "LETTER \"E\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "E" -> (11 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> (10 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "x" -> true | _ -> false),
                      "LETTER \"x\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "x" -> (9 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "F" -> true | _ -> false),
                      "LETTER \"F\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "F" -> (8 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "f" -> true | _ -> false),
                      "LETTER \"f\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "f" -> (7 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "U" -> (6 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> (5 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "I" -> (4 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> (3 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "A" -> (2 : 'sl)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> (1 : 'sl)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (* homo *) (wordsl : 'wordsl Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm (Gram.Entry.obj (sl : 'sl Gram.Entry.t)));
                  Gram.Sopt (Gram.Skeyword ".");
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (w : 'sl list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (w : 'wordsl)
                      | _ -> assert false))) ]) ]))
        ()))
  
let code_raw s = (* VH transliteration *)
  try Gram.parse_string word Loc.ghost s
  with
  | Loc.Exc_located (loc, e) ->
      (Format.eprintf "\nIn string \"%s\", at location %s :\n%!" s
         (Loc.to_string loc);
       raise e)
and code_raw_WX s =
  try Gram.parse_string wordwx Loc.ghost s
  with
  | Loc.Exc_located (loc, e) ->
      (Format.eprintf "\nIn string \"%s\", at location %s :\n%!" s
         (Loc.to_string loc);
       raise e)
and code_raw_KH s =
  try Gram.parse_string wordkh Loc.ghost s
  with
  | Loc.Exc_located (loc, e) ->
      (Format.eprintf "\nIn string \"%s\", at location %s :\n%!" s
         (Loc.to_string loc);
       raise e)
and code_raw_SL s =
  try Gram.parse_string wordsl Loc.ghost s
  with
  | Loc.Exc_located (loc, e) ->
      (Format.eprintf "\nIn string \"%s\", at location %s :\n%!" s
         (Loc.to_string loc);
       raise e)
  
(**************************************************************************)
(* The following gives codes to proper names, starting with upper letters *)
(**************************************************************************)
let upper_lower = Gram.Entry.mk "upper case"
and wordu = Gram.Entry.mk "wordu"
  
let _ =
  (Gram.extend (* skt to nat *) (upper_lower : 'upper_lower Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* duplication with lower necessary in order to get proper sharing of prefix *)
                  (* hiatus *) (* word hiatus for VH trans pra-uga *)
                  Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false), "INT _")) ],
                (Gram.Action.mk
                   (fun (i : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      (let i = Gram.Token.extract_string i
                       in 50 + (int_of_string i) : 'upper_lower))));
               ([ Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> (49 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (48 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "v" -> true | _ -> false),
                      "LETTER \"v\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "v" -> (45 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> (44 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> (43 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "y" -> true | _ -> false),
                      "LETTER \"y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "y" -> (42 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (41 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (36 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "b" -> (39 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "b") -> (40 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "p" -> (37 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "p") -> (38 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> (34 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "d") -> (35 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> (32 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "t") -> (33 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "j" -> true | _ -> false),
                      "LETTER \"j\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "j" -> (24 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "j" -> true | _ -> false),
                      "LETTER \"j\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "j") -> (25 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "c" -> (22 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "c") -> (23 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "g" -> (19 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "g") -> (20 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "k" -> (17 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "k") -> (18 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> (12 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> (10 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> (5 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "u") -> (6 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> (3 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "i") -> (4 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> (1 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "a") -> (13 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "a") -> (11 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "a", LETTER "a") -> (2 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> (16 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (14 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> (9 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> (7 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\""));
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "r", LETTER "r") -> (8 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (31 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (47 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> (29 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "d") -> (30 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> (27 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "t") -> (28 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "."; Gram.Skeyword "."; Gram.Skeyword ".";
                  Gram.Sself ],
                (Gram.Action.mk
                   (fun (c : 'upper_lower) _ _ _ (_loc : Gram.Loc.t) ->
                      (c : 'upper_lower))));
               ([ Gram.Skeyword "'"; Gram.Sself ],
                (Gram.Action.mk
                   (fun (c : 'upper_lower) _ (_loc : Gram.Loc.t) ->
                      (c : 'upper_lower))));
               ([ Gram.Skeyword "$"; Gram.Sself ],
                (Gram.Action.mk
                   (fun (c : 'upper_lower) _ (_loc : Gram.Loc.t) ->
                      (c : 'upper_lower))));
               ([ Gram.Skeyword "_" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> (50 : 'upper_lower))));
               ([ Gram.Skeyword "-" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> (0 : 'upper_lower))));
               ([ Gram.Skeyword "+"; Gram.Sself ],
                (Gram.Action.mk
                   (fun (c : 'upper_lower) _ (_loc : Gram.Loc.t) ->
                      (c : 'upper_lower))));
               ([ Gram.Skeyword "~"; Gram.Skeyword "~" ],
                (Gram.Action.mk
                   (fun _ _ (_loc : Gram.Loc.t) -> (15 : 'upper_lower))));
               ([ Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (26 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> (46 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (46 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "f" -> true | _ -> false),
                      "LETTER \"f\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "f" -> (21 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (21 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "H" -> true | _ -> false),
                      "LETTER \"H\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "H" -> (149 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> (148 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "V" -> true | _ -> false),
                      "LETTER \"V\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "V" -> (145 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "L" -> true | _ -> false),
                      "LETTER \"L\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "L" -> (144 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "R" -> (143 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "Y" -> true | _ -> false),
                      "LETTER \"Y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Y" -> (142 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "M" -> true | _ -> false),
                      "LETTER \"M\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "M" -> (141 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "B" -> true | _ -> false),
                      "LETTER \"B\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "B" -> (139 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "B" -> true | _ -> false),
                      "LETTER \"B\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "B") -> (140 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "P" -> true | _ -> false),
                      "LETTER \"P\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "P" -> (137 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "P" -> true | _ -> false),
                      "LETTER \"P\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "P") -> (138 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "N" -> true | _ -> false),
                      "LETTER \"N\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "N" -> (136 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> (134 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "D") -> (135 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "T" -> (132 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "T") -> (133 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "J" -> true | _ -> false),
                      "LETTER \"J\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "J" -> (124 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "J" -> true | _ -> false),
                      "LETTER \"J\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "J") -> (125 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "C" -> true | _ -> false),
                      "LETTER \"C\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "C" -> (122 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "C" -> true | _ -> false),
                      "LETTER \"C\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "C") -> (123 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "G" -> true | _ -> false),
                      "LETTER \"G\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "G" -> (119 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "G" -> true | _ -> false),
                      "LETTER \"G\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "G") -> (120 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "K" -> true | _ -> false),
                      "LETTER \"K\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "K" -> (117 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "K" -> true | _ -> false),
                      "LETTER \"K\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "K") -> (118 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "O" -> true | _ -> false),
                      "LETTER \"O\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "O" -> (112 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "E" -> true | _ -> false),
                      "LETTER \"E\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "E" -> (110 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> (129 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "D") -> (130 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "T" -> (127 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "T") -> (128 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "R" -> (107 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> (147 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "U" -> (105 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\""));
                  Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "U", LETTER "U") -> (106 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "I" -> (103 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\""));
                  Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "I", LETTER "I") -> (104 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "A" -> (101 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "A") -> (113 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "A") -> (111 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\""));
                  Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "A", LETTER "A") -> (102 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "Z" -> true | _ -> false),
                      "LETTER \"Z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Z" -> (146 : 'upper_lower)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> (146 : 'upper_lower)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (* homo *) (wordu : 'wordu Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm
                       (Gram.Entry.obj
                          (upper_lower : 'upper_lower Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'upper_lower list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (candrabindu_fix w : 'wordu)
                      | _ -> assert false))) ]) ]))
        ()))
  
(* Similar to [code_raw] but accepts upper initials. *)
let code_rawu s =
  try Gram.parse_string wordu Loc.ghost s
  with
  | Loc.Exc_located (loc, e) ->
      (Format.eprintf "\nIn string \"%s\", at location %s :\n%!" s
         (Loc.to_string loc);
       raise e)
  
(******************************************************)
(* Simplified mapping for matching without diacritics *)
(******************************************************)
let simplified = Gram.Entry.mk "simplified"
and wordd = Gram.Entry.mk "wordd"
  
let _ =
  (Gram.extend (* skt to nat *) (simplified : 'simplified Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ (* duplication with lower necessary in order to get proper sharing of prefix *)
                (* glitch prefix candrabindu *) (* hiatus *)
                (* word hiatus for VH trans pra-uga *) (* Vishnou *)
                (* Vichnou , Krichna *) (* consistency with: *) (* Krishna *)
                (* consistency with: *) (* vikruti *)
                Gram.Stoken
                  (((function | LETTER "h" -> true | _ -> false),
                    "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> (49 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (48 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "s") -> (48 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "v" -> true | _ -> false),
                      "LETTER \"v\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "v" -> (45 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> (44 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> (43 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "r") -> (43 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t)
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "u", LETTER "r") ->
                          (43 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "r") -> (43 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_2 : Gram.Token.t)
                      (__camlp4_1 : Gram.Token.t) (__camlp4_0 : Gram.Token.t)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_2, __camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "i", LETTER "r") ->
                          (43 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "y" -> true | _ -> false),
                      "LETTER \"y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "y" -> (42 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (41 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (36 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "b" -> (39 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "b" -> true | _ -> false),
                      "LETTER \"b\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "b") -> (39 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "p" -> (37 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "p" -> true | _ -> false),
                      "LETTER \"p\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "p") -> (37 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> (34 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "d") -> (34 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> (32 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "t") -> (32 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "j" -> true | _ -> false),
                      "LETTER \"j\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "j" -> (24 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "j" -> true | _ -> false),
                      "LETTER \"j\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "j") -> (24 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "c" -> (22 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "c" -> true | _ -> false),
                      "LETTER \"c\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "c") -> (48 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "g" -> (19 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "g" -> true | _ -> false),
                      "LETTER \"g\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "g") -> (19 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "k" -> (17 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "k" -> true | _ -> false),
                      "LETTER \"k\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "k") -> (17 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "o" -> (12 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "o" -> true | _ -> false),
                      "LETTER \"o\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "o") -> (5 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "e" -> true | _ -> false),
                      "LETTER \"e\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "e" -> (10 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "u" -> (5 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "u") -> (5 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "i" -> (3 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "i") -> (3 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "a" -> (1 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "a") -> (13 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "a") -> (11 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\""));
                  Gram.Stoken
                    (((function | LETTER "a" -> true | _ -> false),
                      "LETTER \"a\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "a", LETTER "a") -> (1 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "h" -> (49 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (41 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "l" -> (44 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "r" -> (43 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\""));
                  Gram.Stoken
                    (((function | LETTER "r" -> true | _ -> false),
                      "LETTER \"r\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "r", LETTER "r") -> (43 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (36 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (48 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "d" -> (34 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "d" -> true | _ -> false),
                      "LETTER \"d\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "d") -> (34 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "t" -> (32 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "t" -> true | _ -> false),
                      "LETTER \"t\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "t") -> (32 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword "."; Gram.Skeyword "."; Gram.Skeyword ".";
                  Gram.Snterm
                    (Gram.Entry.obj (upper_lower : 'upper_lower Gram.Entry.t)) ],
                (Gram.Action.mk
                   (fun (c : 'upper_lower) _ _ _ (_loc : Gram.Loc.t) ->
                      (c : 'simplified))));
               ([ Gram.Skeyword "'";
                  Gram.Snterm
                    (Gram.Entry.obj (upper_lower : 'upper_lower Gram.Entry.t)) ],
                (Gram.Action.mk
                   (fun (c : 'upper_lower) _ (_loc : Gram.Loc.t) ->
                      (c : 'simplified))));
               ([ Gram.Skeyword "$";
                  Gram.Snterm
                    (Gram.Entry.obj (upper_lower : 'upper_lower Gram.Entry.t)) ],
                (Gram.Action.mk
                   (fun (c : 'upper_lower) _ (_loc : Gram.Loc.t) ->
                      (c : 'simplified))));
               ([ Gram.Skeyword "_" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> (50 : 'simplified))));
               ([ Gram.Skeyword "-" ],
                (Gram.Action.mk
                   (fun _ (_loc : Gram.Loc.t) -> (0 : 'simplified))));
               ([ Gram.Skeyword "+";
                  Gram.Snterm
                    (Gram.Entry.obj (upper_lower : 'upper_lower Gram.Entry.t)) ],
                (Gram.Action.mk
                   (fun (c : 'upper_lower) _ (_loc : Gram.Loc.t) ->
                      (c : 'simplified))));
               ([ Gram.Skeyword "~"; Gram.Skeyword "~" ],
                (Gram.Action.mk
                   (fun _ _ (_loc : Gram.Loc.t) -> (15 : 'simplified))));
               ([ Gram.Skeyword "~"; Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\""));
                  Gram.Stoken
                    (((function | LETTER "l" -> true | _ -> false),
                      "LETTER \"l\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "l", LETTER "l") -> (150 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword "~";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (36 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "z" -> true | _ -> false),
                      "LETTER \"z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "z" -> (48 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "s" -> true | _ -> false),
                      "LETTER \"s\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "s" -> (48 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "f" -> true | _ -> false),
                      "LETTER \"f\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "f" -> (36 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "n" -> true | _ -> false),
                      "LETTER \"n\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "n" -> (36 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "m" -> true | _ -> false),
                      "LETTER \"m\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "m" -> (15 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "H" -> true | _ -> false),
                      "LETTER \"H\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "H" -> (149 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> (148 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "S") -> (148 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "V" -> true | _ -> false),
                      "LETTER \"V\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "V" -> (145 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "L" -> true | _ -> false),
                      "LETTER \"L\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "L" -> (144 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "R" -> (143 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "Y" -> true | _ -> false),
                      "LETTER \"Y\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Y" -> (142 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "M" -> true | _ -> false),
                      "LETTER \"M\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "M" -> (141 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "B" -> true | _ -> false),
                      "LETTER \"B\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "B" -> (139 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "B" -> true | _ -> false),
                      "LETTER \"B\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "B") -> (139 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "P" -> true | _ -> false),
                      "LETTER \"P\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "P" -> (137 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "P" -> true | _ -> false),
                      "LETTER \"P\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "P") -> (137 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "N" -> true | _ -> false),
                      "LETTER \"N\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "N" -> (136 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> (134 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "D") -> (134 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "T" -> (132 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "T") -> (132 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "J" -> true | _ -> false),
                      "LETTER \"J\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "J" -> (124 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "J" -> true | _ -> false),
                      "LETTER \"J\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "J") -> (124 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "C" -> true | _ -> false),
                      "LETTER \"C\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "C" -> (122 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "C" -> true | _ -> false),
                      "LETTER \"C\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "C") -> (122 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "G" -> true | _ -> false),
                      "LETTER \"G\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "G" -> (119 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "G" -> true | _ -> false),
                      "LETTER \"G\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "G") -> (119 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "K" -> true | _ -> false),
                      "LETTER \"K\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "K" -> (117 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "K" -> true | _ -> false),
                      "LETTER \"K\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "K") -> (117 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "O" -> true | _ -> false),
                      "LETTER \"O\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "O" -> (112 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "E" -> true | _ -> false),
                      "LETTER \"E\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "E" -> (110 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "D" -> (134 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "D" -> true | _ -> false),
                      "LETTER \"D\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "D") -> (134 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "T" -> (132 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "T" -> true | _ -> false),
                      "LETTER \"T\""));
                  Gram.Stoken
                    (((function | LETTER "h" -> true | _ -> false),
                      "LETTER \"h\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "h", LETTER "T") -> (132 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "R" -> true | _ -> false),
                      "LETTER \"R\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "R" -> (143 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword ".";
                  Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> (148 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "U" -> (105 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\""));
                  Gram.Stoken
                    (((function | LETTER "U" -> true | _ -> false),
                      "LETTER \"U\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "U", LETTER "U") -> (105 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "I" -> (103 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\""));
                  Gram.Stoken
                    (((function | LETTER "I" -> true | _ -> false),
                      "LETTER \"I\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "I", LETTER "I") -> (103 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "A" -> (101 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\""));
                  Gram.Stoken
                    (((function | LETTER "u" -> true | _ -> false),
                      "LETTER \"u\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "u", LETTER "A") -> (113 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\""));
                  Gram.Stoken
                    (((function | LETTER "i" -> true | _ -> false),
                      "LETTER \"i\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "i", LETTER "A") -> (111 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\""));
                  Gram.Stoken
                    (((function | LETTER "A" -> true | _ -> false),
                      "LETTER \"A\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (LETTER "A", LETTER "A") -> (101 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Stoken
                    (((function | LETTER "Z" -> true | _ -> false),
                      "LETTER \"Z\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "Z" -> (148 : 'simplified)
                      | _ -> assert false)));
               ([ Gram.Skeyword "\"";
                  Gram.Stoken
                    (((function | LETTER "S" -> true | _ -> false),
                      "LETTER \"S\"")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | LETTER "S" -> (148 : 'simplified)
                      | _ -> assert false))) ]) ]))
        ());
   Gram.extend (wordd : 'wordd Gram.Entry.t)
     ((fun () ->
         (None,
          [ (None, None,
             [ ([ Gram.Slist0
                    (Gram.Snterm
                       (Gram.Entry.obj
                          (simplified : 'simplified Gram.Entry.t)));
                  Gram.Skeyword "#";
                  Gram.Stoken
                    (((function | INT ((_)) -> true | _ -> false),
                      "INT ((_))"));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_1 : Gram.Token.t)
                      (__camlp4_0 : Gram.Token.t) _ (w : 'simplified list)
                      (_loc : Gram.Loc.t) ->
                      match (__camlp4_1, __camlp4_0) with
                      | (EOI, INT ((_))) -> (w : 'wordd)
                      | _ -> assert false)));
               ([ Gram.Slist0
                    (Gram.Snterm
                       (Gram.Entry.obj
                          (simplified : 'simplified Gram.Entry.t)));
                  Gram.Stoken
                    (((function | EOI -> true | _ -> false), "EOI")) ],
                (Gram.Action.mk
                   (fun (__camlp4_0 : Gram.Token.t) (w : 'simplified list)
                      (_loc : Gram.Loc.t) ->
                      match __camlp4_0 with
                      | EOI -> (candrabindu_fix w : 'wordd)
                      | _ -> assert false))) ]) ]))
        ()))
  
(* homo index ignored *)
(* Similar to [code_skt_ref] but simplified (no diacritics) *)
let code_rawd s =
  try Gram.parse_string wordd Loc.ghost s
  with
  | Loc.Exc_located (loc, e) ->
      (Format.eprintf "\nIn string \"%s\", at location %s :\n%!" s
         (Loc.to_string loc);
       raise e)
  

