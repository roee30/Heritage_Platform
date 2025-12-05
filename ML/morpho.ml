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
open Skt_morph
  
open Morphology
  
(* [inflected] and its constructors [Noun_form], ..., [homo_krid]  *)
open Naming
  
(* [homo_undo look_up_homo unique_kridantas lexical_kridantas] *)
open Morpho_string
  
(* [string_morph string_verbal] *)
module Morpho_out (Chan : sig val chan : out_channel ref
                                 end) =
  struct
    let ps s = output_string !Chan.chan s
      
    let pl s = (s ^ "\n") |> ps
      
    let pr_word w = (Canon.decode w) |> ps
      
    let print_morph m = (string_morph m) |> ps
    and print_verbal vb = (string_verbal vb) |> ps
      
    (*i Anomaly: Morpho should be independent of Html i*)
    let select_morph (seg_num, sub, seg_count) morph =
      ((let string_num = string_of_int seg_num
        and seg = (string_of_int sub) ^ ("," ^ (string_of_int seg_count)) in
        let radio_cond = Html.radio_input_dft string_num seg ""
        in
          match (sub, seg_count) with
          | (1, 1) -> ((radio_cond true) ^ " ") |> ps
          | (* NB: only the first button is selected *) _ ->
              ((radio_cond false) ^ " ") |> ps);
       print_morph morph)
      
    let rec select_morphs (seg_num, sub) seg_count =
      function
      | [] -> ()
      | [ last ] -> select_morph (seg_num, sub, seg_count) last
      | first :: rest ->
          (select_morph (seg_num, sub, seg_count) first;
           " | " |> ps;
           select_morphs (seg_num, sub) (seg_count + 1) rest)
      
    let print_morphs (seg_num, sub) morphs =
      match seg_num with
      | 0 ->
          let bar () = " | " |> ps
          in List2.process_list_sep print_morph bar morphs
      | _ -> select_morphs (seg_num, sub) 1 morphs
      
    let ddv x = (ps "XXX "; ps x; ps " YYY"; x)
      
    let dd x = (ps "XXX"; x)
      
    (* The following print functions insert in the HTML output links to entries 
   in the lexicon, also radio buttons and other marks for user choices. *)
    open Debug
      
    (* [pe : word -> unit] is [Morpho_html.print_entry] with hyperlink,
   [pne : word -> unit] is [Morpho_html.print_stem],
   [pu : word -> unit] prints un-analysed chunks. *)
    let print_inv_morpho pe pne pu form (seg_num, sub) generative
                         (delta, morphs) =
      let stem = Word.patch delta form
      in
        ((* stem may have homo index *)
         "[" |> ps;
         if generative
         then (* interpret stem as unique name *)
           (let (homo, bare_stem) = homo_undo stem in
            let krit_infos = Deco.assoc bare_stem unique_kridantas
            in
              try
                let (verbal, root) = look_up_homo homo krit_infos
                in
                  ((match Deco.assoc bare_stem lexical_kridantas with
                    | [] -> (* not in lexicon *)
                        if stem = [ 3; 32; 1 ]
                        then (* ita ifc *) stem |> pe
                        else bare_stem |> pne
                    | entries -> (* bare stem is lexicalized *)
                        if List.exists (fun (_, h) -> h = homo) entries
                        then stem |> pe
                        else (* stem with exact homo is lexical entry *)
                          bare_stem |> pne);
                   " { " |> ps;
                   print_verbal verbal;
                   " }[" |> ps;
                   root |> pe;
                   "]" |> ps)
              with | _ -> bare_stem |> pu)
         else
           (match morphs with
            | [ Unanalysed ] -> stem |> pu
            | _ -> stem |> pe);
         "]{" |> ps;
         print_morphs (seg_num, sub) morphs;
         "}" |> ps)
      
    (* Decomposes a preverb sequence into the list of its components *)
    let decomp_pvs pvs = Deco.assoc pvs Naming.preverbs_structure
      
    (* Used in [Morpho_html] *)
    let print_inv_morpho_link (pvs : int list) pe pne pu form =
      let pv =
        if Phonetics.phantomatic form
        then [ 2 ]
        else (* aa- *) (*i OBSOLETE i*) pvs in
      let encaps print (e : int list) =
        (* encapsulates prefixing with possible preverbs *)
        if pv = [] then print e else pe (pv @ e)
      in print_inv_morpho (encaps pe) (encaps pne) pu form
      
  end
  

