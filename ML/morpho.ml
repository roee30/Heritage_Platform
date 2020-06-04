(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Prints morphological information, including derivative morphology.
   Used in [Morpho_html] and [Morpho_ext] *)

open Skt_morph;
open Morphology; 
  (* [inflected] and its constructors [Noun_form], ..., [homo_krid]  *)
open Naming; (* [homo_undo look_up_homo unique_kridantas lexical_kridantas] *) 
open Morpho_string (* [string_morph string_verbal] *);

module Morpho_out (Chan: sig value chan: ref out_channel; end)  
 = struct 

value ps s = output_string Chan.chan.val s 
; 
value pl s = s ^ "\n" |> ps
;
value pr_word w = Canon.decode w |> ps
;
value print_morph m = string_morph m |> ps
and print_verbal vb = string_verbal vb |> ps
;
(*i Anomaly: Morpho should be independent of Html i*)
value select_morph (seg_num,sub,seg_count) morph = do 
  { let string_num = string_of_int seg_num  
    and seg = (string_of_int sub) ^ "," ^ (string_of_int seg_count) in
    let radio_cond = Html.radio_input_dft string_num seg "" in
    match (sub,seg_count) with 
    [ (1,1) -> radio_cond True ^ " " |> ps 
                  (* NB: only the first button is selected *)
    | _     -> radio_cond False ^ " " |> ps
    ]
  ; print_morph morph
  }
;
value rec select_morphs (seg_num,sub) seg_count = fun
  [ [] -> ()
  | [ last :: [] ] -> select_morph (seg_num,sub,seg_count) last 
  | [ first :: rest ] -> do
      { select_morph (seg_num,sub,seg_count) first
      ; " | " |> ps
      ; select_morphs (seg_num,sub) (seg_count+1) rest
      }
  ]
;
value print_morphs (seg_num,sub) morphs = match seg_num with
  [ 0 -> let bar () = " | " |> ps in
         List2.process_list_sep print_morph bar morphs
  | _ -> select_morphs (seg_num,sub) 1 morphs
  ]
; 
(* The following print functions insert in the HTML output links to entries 
   in the lexicon, also radio buttons and other marks for user choices. *)

(* [pe : word -> unit] is [Morpho_html.print_entry] with hyperlink,
   [pne : word -> unit] is [Morpho_html.print_stem],
   [pu : word -> unit] prints un-analysed chunks. *)
value print_inv_morpho pe pne pu form (seg_num,sub) generative (delta,morphs) = 
  let stem = Word.patch delta form in do (* stem may have homo index *)
    { "[" |> ps 
    ; if generative then (* interpret stem as unique name *)
        let (homo,bare_stem) = homo_undo stem in
        let krit_infos = Deco.assoc bare_stem unique_kridantas in 
        try let (verbal,root) = look_up_homo homo krit_infos in do
        { match Deco.assoc bare_stem lexical_kridantas with
          [ [] (* not in lexicon *) -> 
              if stem = [ 3; 32; 1 ] (* ita ifc *) then stem |> pe
                                                   else bare_stem |> pne
          | entries (* bare stem is lexicalized *) ->
              if List.exists (fun (_,h) -> h=homo) entries
                 then stem |> pe (* stem with exact homo is lexical entry *)
              else bare_stem |> pne
          ] 
        ; " { " |> ps; print_verbal verbal; " }[" |> ps; root |> pe; "]" |> ps
        } with [ _ -> bare_stem |> pu ]
      else match morphs with
	   [ [ Unanalysed ] -> stem |> pu 
	   | _ -> stem |> pe
	   ]
    ; "]{" |> ps
    ; print_morphs (seg_num,sub) morphs
    ; "}" |> ps
    }
;
(* Decomposes a preverb sequence into the list of its components *)
value decomp_pvs pvs = 
  Deco.assoc pvs Naming.preverbs_structure
;
(* Used in [Morpho_html] *)
value print_inv_morpho_link pvs pe pne pu form =
  let pv = if Phonetics.phantomatic form then [ 2 ] (* aa- *)(*i OBSOLETE i*)
           else pvs in
  let encaps print e = (* encapsulates prefixing with possible preverbs *)
     if pv = [] then print e 
     else let pr_pv pv = do { pv |> pe; "-" |> ps } in do 
              { List.iter pr_pv (decomp_pvs pvs); print e } in
          print_inv_morpho (encaps pe) (encaps pne) pu form
(* Possible overgeneration when derivative of a root non attested with pv 
   since only existential test in [Dispatcher.validate_pv]. Thus
   [anusandhiiyate] should show [dhaa#1], not [dhaa#2], [dhii#1] or [dhyaa] *)
;

(* Used in [Lexer.record_tagging] for regression analysis *)
value report_morph gen form (delta,morphs) =
  let stem = Word.patch delta form in do (* stem may have homo index *)
    { "{ " |> ps
    ; print_morphs (0,0) morphs 
    ; " }[" |> ps 
    ; if gen then (* interpret stem as unique name *)
        let (homo,bare) = homo_undo stem in
        let krid_infos = Deco.assoc bare unique_kridantas in 
        let (vb,root) = look_up_homo homo krid_infos in do
        { match Deco.assoc stem lexical_kridantas with
          [ [] (* not in lexicon *)      -> do { "G:" |> ps; pr_word bare }
          | _  (* stem is lexicalized *) -> do { "L:" |> ps; pr_word stem }
          ]
        ; " { " |> ps; print_verbal vb; " }[" |> ps; pr_word root; "]" |> ps
        }
      else pr_word stem
    ; "]" |> ps
    } 
;

end;

