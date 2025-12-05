(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* CGI-bin indexer for indexing in sanskrit dictionary.                   *)
(* This CGI is triggered by page [index.html] in [dico_dir].              *)
(* Reads its input in shell variable [QUERY_STRING] URI-encoded.          *)
(*i Test with: QUERY_STRING = "q=yoga"; ./indexer                        i*)
(*i Web invocation is http://skt_server_url/cgi-bin/sktindex?query=str   i*)
(*i module Indexer = struct i*)
open Html
  
(* [table_begin] etc. *)
open Web
  
(* ps pl abort etc. *)
open Cgi
  
let answer_begin () =
  (pl (table_begin Yellow_cent); ps tr_begin; ps th_begin)
  
let answer_end () = (ps th_end; ps tr_end; pl table_end; pl html_paragraph)
  
let ok (mess, s) = (ps mess; pl (Morpho_html.skt_anchor false s))
and ok2 (mess, s1, s2) = (ps mess; pl (Morpho_html.skt_anchor2 s1 s2))
  
(* ok2 prints the entry under the spelling given by the user, i.e. without 
        normalisation, thus e.g. sandhi is not written sa.mdhi, and possibly 
        suffixed by homonymy index 1, e.g. b.rh. *)
(* Should share [Lemmatizer.load_inflected] *)
let load_inflected file : Morphology.inflected_map = Gen.gobble file
  
let load_nouns () = load_inflected Data.public_nouns_file
and load_roots () = load_inflected Data.public_roots_file
and load_vocas () = load_inflected Data.public_vocas_file
and load_indecls () = load_inflected Data.public_inde_file
and load_parts () = load_inflected Data.public_parts_file
  
let back_ground = background Chamois
  
let display word l =
  (ps " found as inflected form:";
   pl html_break;
   let pi inv = Morpho_html.print_inflected false word inv in List.iter pi l)
and report_failure s =
  (ps " not found in dictionary";
   pl html_break;
   ps "Closest entry in lexical order: ";
   ps (Morpho_html.skt_anchor false s);
   pl html_break)
  
let try_declensions word before =
  (* before is last lexical item before word in lexical order *)
  (* This is costly because of the size of inverted inflected databases *)
  let inflectedn = load_nouns ()
  in
    match Deco.assoc word inflectedn with
    | [] -> (* Not found; we try vocative forms *)
        let inflectedv = load_vocas ()
        in
          (match Deco.assoc word inflectedv with
           | [] -> (* Not found; we try root forms *)
               let inflectedr = load_roots ()
               in
                 (match Deco.assoc word inflectedr with
                  | [] -> (* Not found; we try adverbial forms *)
                      let inflecteda = load_indecls ()
                      in
                        (match Deco.assoc word inflecteda with
                         | [] -> report_failure before
                         | (* NB - no look-up in parts forms since big and partly lexicalized *)
                             l -> display word l)
                  | l -> display word l)
           | l -> display word l)
    | l -> display word l
  
let print_word_unique word (entry, lex, page) = (* lex="other" allowed *)
  let link = Morpho_html.skt_anchor_M word entry page false
  in pl (link ^ (" [ " ^ (lex ^ (" ]" ^ (xml_empty "br")))))
  
(* this allows access to a pseudo-entry such as "hvaaya" *)
let print_word word (entry, lex, page) =
  match lex with
  | "other" -> ()
  | _ -> print_word_unique word (entry, lex, page)
  
let read_mw_index () : (string * string * string) Deco.deco =
  Gen.gobble Data.public_mw_index_file
  
let skt_red s = html_red (Morpho_html.skt_roma s)
  
let index_engine () =
  (pl http_header;
   page_begin heritage_dictionary_title;
   pl (body_begin back_ground);
   let query = Sys.getenv "QUERY_STRING" in
   let env = create_env query in
   let translit = get "t" env Paths.default_transliteration
   and lex = get "lex" env Paths.default_lexicon
   and (* default by config *) url_encoded_entry = get "q" env "" in
   let lang = language_of_string lex
   in
     (print_title_solid Mauve (Some lang) (dico_title lang);
      let str = decode_url url_encoded_entry
      and (* in translit *) encode = Encode.switch_code translit
      and () = toggle_lexicon lex
      in
        try
          let word = encode str in (* normalization *)
          let str_VH = Canon.decode word
          in
            (answer_begin ();
             ps (div_begin Latin12);
             (match lex with
              | "MW" ->
                  let mw_index = read_mw_index () in
                  let words = Deco.assoc word mw_index
                  in
                    (match words with
                     | [] ->
                         (ps (skt_red str_VH);
                          ps " not found in MW dictionary";
                          pl html_break)
                     | [ unique ] -> print_word_unique str_VH unique
                     | _ -> List.iter (print_word str_VH) (List.rev words))
              | "SH" -> (* richer search engine *)
                  let sh_index = Index.read_entries ()
                  in
                    (try
                       let (s, b, h) = Index.search word sh_index
                       in
                         if b || h
                         then
                           (let r = Canon.decode word in
                            let hr = if h then r ^ "_1" else r
                            in ok2 ("Entry found: ", s, hr))
                         else ok ("First matching entry: ", s)
                     with
                     | (* remark that s may be str with some suffix,   *)
                         (* even though str may exist as inflected form  *)
                         (* Matching entry not found - we try declensions *)
                         (* This occurs rarely. Example: "vi.s.no" *)
                         Index.Last last ->
                         (ps (skt_red str_VH); try_declensions word last))
              | _ -> failwith "Unknown lexicon");
             ps div_end;
             (* Latin12 *)
             answer_end ();
             page_end lang true)
        with
        | (* do *) Stream.Error _ ->
            abort lang "Illegal transliteration " str))
  
(* do *)
(* do *)
let safe_index_engine () =
  let abor = abort Html.French
  in
    (* may not preserve the current language *)
    try index_engine ()
    with | Sys_error s -> abor Control.sys_err_mess s
    | (* file pb *) Stream.Error s -> abor Control.stream_err_mess s
    | (* file pb *) Invalid_argument s -> abor Control.fatal_err_mess s
    | (* sub *) Failure s -> abor Control.fatal_err_mess s
    | (* anomaly *) Control.Fatal s -> abor Control.fatal_err_mess s
    | (* anomaly *) Not_found -> abor Control.fatal_err_mess "assoc"
    | (* assoc *) End_of_file -> abor Control.fatal_err_mess "EOF"
    | (* EOF *) Encode.In_error s -> abor "Wrong_input " s
    | Exit -> abor "Wrong character in input - " "use ASCII"
    | (* Sanskrit *) _ -> abor Control.fatal_err_mess "Unexpected anomaly"
  
(* ? *)
(* typical invocation is [http://skt_server_url/cgi-bin/sktindex?t=VH&lex=SH&q=input] *)
let _ = safe_index_engine ()
  

