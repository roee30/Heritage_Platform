(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Similar to Tagger but returns tagging if unique, fails otherwise.
   More general than Lemmatizer, since it analyses forms of verbs with 
   preverbs and not just root forms; also allows for proper nouns.
   Prints warnings on stdout.
   Used by [Tag_tree] (Experimental). *)

(* 2020: This needs adaptation to current morphology *)

(*i module Stemmer = struct i*)
open Transduction;
open Canon;
open Dico.Dictionary;
open Morphology; (* [inflected inflected_map] *)
open Cgi;
open Segmenter; (* [segment1_initial continue] *)
open Dispatch; (* Phases nominal verbal etc *) 
open Auto.Auto;

value give_up cat = 
  let mess = "Missing " ^ cat ^ " automaton" in do
  { failwith ("System error - please report - " ^ mess)
  ; (* installation problem -- executing process fails *)
  }
;
value full=False (* how to call Dispatch with parameters Full=False/True? *)
;
(* Load persistent transducer automaton of given phase (lexical category). *)
(* These files have been copied from their development version [transn_file]
   etc. created by [Make_inflected] followed by [Make_automaton]. *)

open Load_transducers
;
module Prel = struct
  value prelude () = ();
end
;
module Transducers = Trans Prel
;
module Disp = Dispatch Transducers
;
open Phases (* defines [phase] *)
;
open Disp 
;
module Control = struct
  value star = ref False; (* chunk = word (pada) *)
end;

module Viccheda = Segmenter.Segment Phases Disp Control
;
open Viccheda (* [segment1_initial] [continue] *)
;

(* We follow the structure of Lexer *)

value load_morpho file = 
  try (Gen.gobble file : inflected_map)
  with [ _ -> give_up "morphology" ]
and load_preverbs file = 
  try (Gen.gobble file : Deco.deco Word.word) 
  with [ _ -> give_up "preverbs" ]
;
value load_morphs () = 
  { nouns = load_morpho Data.public_nouns_file
  ; prons = load_morpho Data.public_pronouns_file
  ; roots = load_morpho Data.public_roots_file
  ; krids = load_morpho Data.public_parts_file
  ; voks  = load_morpho Data.public_partvocs_file
  ; lopas = load_morpho Data.public_lopas_file
  ; lopaks = load_morpho Data.public_lopaks_file
  ; undes = load_morpho Data.public_adverbs_file
  ; absos = load_morpho Data.public_absols_file
  ; iics  = load_morpho Data.public_iics_file
  ; iifs  = load_morpho Data.public_iifcs_file
  ; iiks  = load_morpho Data.public_piics_file
  ; iivs  = load_morpho Data.public_iivs_file
  ; iiys  = load_morpho Data.public_avyayais_file
  ; avys  = load_morpho Data.public_avyayafs_file
  ; peris = load_morpho Data.public_peris_file
  ; auxis = load_morpho Data.public_auxis_file
  ; auxiinvs = load_morpho Data.public_auxiinvs_file
  ; auxiks = load_morpho Data.public_auxiks_file
  ; auxiicks = load_morpho Data.public_auxiicks_file
  ; vocas = load_morpho Data.public_vocas_file
  ; invs  = load_morpho Data.public_invs_file
  ; ifcs  = load_morpho Data.public_ifcs_file
  ; sfxs  = load_morpho Data.public_sfxs_file
  ; isfxs = load_morpho Data.public_isfxs_file
  ; caches = load_morpho Data.public_cache_file
  ; cacheis = load_morpho Data.public_cachei_file
  ; prevs = load_preverbs Data.public_preverbs_file
  } 
;
value morph = load_morphs () (* loading morphology databases *)
;
exception Solution of Word.word
;
exception Solution2 of Word.word and Word.word
;
exception Finished
;
(* looks for first segmentation in one segment *)
value rec search_unit_solution (output,cont) =
  match List.rev output with 
     [ [ (_,word,_) ] -> raise (Solution word) (* one segment *)
     | [ (_,word1,_); (_,word2,_) ] -> raise (Solution2 word1 word2) (* 2 segments *)
     | _ -> match continue cont with (* Note : continue is generic over sorts *)
            [ Some p -> search_unit_solution p
            | None -> raise Finished
            ]
     ] 
;
(* remove initial upper case letter *) (* incomplete .R .S *)
value lower = fun
  [ [ c :: l ] -> let c' = if c>100 then c-100 else c in [ c' :: l ]
  | [] -> []
  ]
;
value verbal = (* root verbal forms *)
  [ Root
  ; Abso  (* abs-ya with aa preverb *)
  ; Pv
  ; Pvk
  ]
and nominal =        
  [ Noun 
  ; A; An
  ; Ifc 
  ; Pron
  ; Krid 
  ; Voca; Inv
  ; Iic 
  ; Iik
  ]
and iic =        
  [ Iic  
  ; Iik 
  ; A; An
  ]
and iiv =        
  [ Iiv ]
and indeclinable = [ Inde; Abso ]
and participial = [ Krid; Pvk; Iik ]
;
(* clause sort *)
value label entry = 
  if entry = nominal then "nominal" else
  if entry = verbal then "verbal" else
  if entry = participial then "participial" else
  if entry = iic then "iic" else
  if entry = indeclinable then "indeclinable" else
  if entry = iiv then "iiv" else
  failwith "Unknown entry for stemmer"
;
value warn0 sort word =
  let message = "No " ^ label sort ^ " solution for " ^ Canon.decode word ^ "\n" in
  output_string stdout message

and warn2 sort word =
  let message = "No unit " ^ label sort ^ " solution for " 
                ^ Canon.decode word ^ "\n" in
  output_string stdout message

;
(* [sort : list phase] is the lexical sort of the lemmatized item we stem *)
value stem sort ident = (* phases in sort must be accepting *)
  let word = Encode.normalize (lower ident) in
  match segment1_initial sort word with 
  [ Some p -> try search_unit_solution p with
         [ Finished ->  do { warn2 sort word; "?" }
         | Solution word -> Canon.decode_ref (List.rev word) 
         | Solution2 word1 word2 -> 
             let s1 = Canon.decode_ref (Word.mirror word1)
             and s2 = Canon.decode_ref (Word.mirror word2) in
             s1 ^ "2" ^ s2 ^ "\n" 
         ]
  | None -> do { warn0 sort word; "0" }
  ]
;
(* necessary since iic is not accepting *)
value search_iic ident = 
  let word = Encode.normalize (lower ident) in 
  match Deco.assoc word morph.iics with
    [ [] -> do { warn0 iic word; "?" }
    | _ -> Canon.decode_ref word 
    ]
;
value search_iiv ident = 
  let word = Encode.normalize (lower ident) in 
  match Deco.assoc word morph.iivs with
    [ [] -> do { warn0 iiv word; "?" }
    | _ -> Canon.decode_ref word 
    ]
;
value injunctions = (* NB: dhik treated otherwise now *)
  [ "aye"; "are"; "aho"; "aa.h"; "prasiida"; "svasti"; "bho"; "bho.h"; 
    "hanta"; "haa" ] 
;
value nominal_stem case = stem nominal (* to do check case Noun+Ifc *)
and verbal_stem = stem verbal 
and participial_stem case = stem participial (* to do check case *)
and iic_stem = search_iic 
and iiv_stem = search_iiv
and indeclinable_stem = stem indeclinable
;


