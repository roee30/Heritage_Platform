(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* module Web html = struct *)

(* Module Web reads localisation parameters from paths.ml, 
   created by "make configure" in main directory, called by configure script.
   Describes all installation parameters and resources other than Inastall. *)

(*  Dynamic html rendering, used by cgis *)

open Html;

(* truncation is the maximum number of solutions computed by the lexer.
   Too small a truncation limit will miss solutions, too large a truncation 
   limit will provoke un unrecoverable choking server failure. This is relevant
   only for the parser (deprecated) mode. The graph interface has no limit. *)
value truncation = 10000 
;
 
(* threshold for printing the list of explicit segmentation solutions *)
value max_count = 100 (* do not exceed - use rather the graphical interface *)
;
value cache_allowed = target=Station (* cache allowed only on Station *)
;
value cache_active = ref (if cache_allowed then "t" else "f")
;
(* For interface look-and-feel *) 
value (display_morph_action,mouse_action_help) = match Paths.mouse_action with
  [ "CLICK" -> ("onclick","Click") 
  | "OVER"  -> ("onMouseover","Mouse")
  | _ -> failwith "Unknown mouse action, change config file"
  ]
;
value cgi_bin name = Paths.cgi_dir_url ^ name
;
(* Call-backs as cgi binaries *)
value index_cgi    = cgi_bin Paths.cgi_index      (* index *)
and dummy_cgi      = cgi_bin Paths.cgi_indexd     (* index for dummies *)
and decls_cgi      = cgi_bin Paths.cgi_decl       (* declensions *) 
and conjs_cgi      = cgi_bin Paths.cgi_conj       (* conjugations *) 
and lemmatizer_cgi = cgi_bin Paths.cgi_lemmatizer (* lemmatizer *) 
and reader_cgi     = cgi_bin Paths.cgi_reader     (* reader *) 
and parser_cgi     = cgi_bin Paths.cgi_parser     (* parser *) 
and graph_cgi      = cgi_bin Paths.cgi_graph      (* summarizer graphical interface *) 
and user_aid_cgi   = cgi_bin Paths.cgi_user_aid   (* unknown chunks processing *) 
and sandhier_cgi   = cgi_bin Paths.cgi_sandhier   (* sandhier *) 
and corpus_manager_cgi = cgi_bin Paths.cgi_corpus_manager (* Corpus manager *)
and save_corpus_cgi = cgi_bin Paths.cgi_save_corpus
;
(* Absolute paths on development site *)
value resources name = Paths.skt_resources_dir ^ name ^ "/"
;
(* Read-only resources *)
value heritage_dir  = resources "DICO" 
and data_dir        = resources "DATA" 
; 
(* Local resources *)
value top_dev_dir name = Paths.skt_install_dir ^ name ^ "/" 
; 
value dico_dir = top_dev_dir "DICO" (* augments local copy of DICO dynamically *)
;
(* Absolute paths of target server *)
value top_site_dir name = Paths.public_skt_dir ^ name ^ "/"
;
value public_dico_dir = top_site_dir "DICO" (* hypertext dictionary *)
and public_data_dir   = top_site_dir "DATA" (* linguistic data for cgis *)
and var_dir           = top_site_dir "VAR" (* Parser dynamic regression suites *)
and corpus_dir        = top_site_dir "CORPUS" (* Corpus tree *)
;
(* This file is accessible only from Station clients in [var_dir] *)
value regression_file_name = "regression" (* regression analysis stuff *)
;

value data name = data_dir ^ name
and dico_page name = dico_dir ^ name
and public_data name = public_data_dir ^ name
and public_dico_page name = public_dico_dir ^ name
;
value public_entries_file = public_dico_page "entries.rem" 
(* created by [make releasedata], read by [indexer] *)
and public_dummies_file = public_dico_page "dummies.rem" 
(* created by [make releasedata], read by [indexerd] *)
;
value sandhis_file = public_data "sandhis.rem"  
;                              
value nouns_file = data "nouns.rem"  
    (* created by [make_nouns], read by [Print_inflected.read_nouns],
       copied in [public_nouns_file] by make releasecgi for use by cgis *)
and nouns2_file = data "nouns2.rem"  (* same in mode non gen *) 
and pronouns_file = data "pronouns.rem" 
    (* created by [make_nouns], read by [Print_inflected.read_pronouns] *)
and roots_infos_file = data "roots_infos.rem" 
    (* created by [Print_dict.postlude], read by [Make_roots.make_roots] *)
and roots_usage_file = data "roots_usage.rem" 
    (* created by [Print_html.postlude], read by [Dispatcher.roots_usage] *)
and verblinks_file = data "verblinks.rem" 
    (* created by [Print_dict.postlude] calling [Roots.collect_preverbs] *)
    (* read by [Print_html], [Make_preverbs] *)
    (* copied in [public_verblinks_file] *)
and lexical_kridantas_file = data "lexical_kridantas.rem" 
    (* created by [Print_dict.postlude]
       read by [Make_roots.roots_to_conjugs] *)   
and unique_kridantas_file = data "unique_kridantas.rem" 
(* created by [Make_roots.roots_to_conjugs] *)  
and roots_file    = data "roots.rem" 
    (* created by [make_roots], read by [reader], [tagger] \& [indexer] *)
and peris_file    = data "peris.rem" 
and lopas_file    = data "lopas.rem" 
and parts_file    = data "parts.rem" 
and partvocs_file = data "partvocs.rem" 
and lopaks_file   = data "lopaks.rem" 
and preverbs_file = data "preverbs.rem" 
    (* created by [make_preverbs], read by [make_inflected] *)
and preverbs_textfile trans = data (trans ^ "_preverbs.txt")
    (* created by [make_preverbs] for documentation *)
and iics_file     = data "iics.rem" 
    (* created by [make_nouns], copied in [public_iics_file] by make install,
       read by [make_automaton] invoked from DATA/Makefile *)
and iics2_file    = data "iics2.rem" (* same in mode non gen *)
and iifcs_file    = data "iifcs.rem" (* iic stems of ifc nouns *)
and vocas_file    = data "voca.rem"  (* created by [make_nouns] etc. *)
and invs_file     = data "invs.rem"  (* created by [make_nouns] etc. *)
and piics_file    = data "piics.rem" (* created by [make_roots] etc. *)
and ifcs_file     = data "ifcs.rem"  (* created by [make_nouns] etc. *)
and ifcs2_file    = data "ifcs2.rem" (* same in mode non gen *)
and avyayais_file = data "avyayais.rem" (* iic stems of avyayiibhava cpds *)
and avyayafs_file = data "avyayafs.rem" (* ifc stems of avyayiibhava cpds *)
and sfxs_file     = data "sfxs.rem"  (* created by [make_nouns] etc. *)
and isfxs_file    = data "isfxs.rem"  (* created by [make_nouns] etc. *)
and iivs_file     = data "iivs.rem"  (* created by [make_roots] etc. *)
and auxis_file    = data "auxi.rem"  (* created by [make_roots] etc. *)
and auxiks_file   = data "auxik.rem"  (* created by [make_roots] etc. *)
and auxiicks_file = data "auxiick.rem"  (* created by [make_roots] etc. *)
and indecls_file  = data "indecls.rem" (* created by  [make_roots] etc. *)
and absya_file    = data "absya.rem"  (* created by [make_roots] etc. *)
and abstvaa_file  = data "abstvaa.rem"  (* created by [make_roots] etc. *)
and inftu_file    = data "inftu.rem"  (* created by [make_roots] etc. *)
and kama_file     = data "kama.rem"  (* created by [make_nouns] etc. *)
and cache_file    = data "cache.rem"

(* Then transducers files, made by [make_automaton], invoked by DATA/Makefile *)
(* NB The [transxxx_file] identifiers are just here for documentation,
   but are not used in the ML code, since the corresponding files are
   created by [make_automaton] when [make transducers] is called in DATA/Makefile
   and copied as [public_transxxx_file] on the server by [make releasedata].
   But [public_transxxx_file] is read by [Load_transducers]. 
   It would be clearer to have a module [Dump_transducers] using them. *)
and transn_file       = data "transn.rem"       (* [noun_automaton] *) 
and transn2_file      = data "transn2.rem"      (* [noun2_automaton] *) 
and transpn_file      = data "transpn.rem"      (* [pronoun_automaton] *) 
and transr_file       = data "transr.rem"       (* [root_automaton] *)
and transperi_file    = data "transperi.rem"    (* [peri_automaton] *)
and translopa_file    = data "translopa.rem"    (* [eoroot_automaton] *)
and transp_file       = data "transp.rem"       (* [preverb_automaton] *)
and transpa_file      = data "transpa.rem"      (* [part_automaton] *)
and translopak_file   = data "translopak.rem"   (* [eopart_automaton] *)
and transpav_file     = data "transpav.rem"     (* [partv_automaton] *)
and transic_file      = data "transic.rem"      (* [iic_automaton] *)
and transic2_file     = data "transic2.rem"     (* [iic2_automaton] *)
and transpic_file     = data "transpic.rem"     (* [piic_automaton] *)
and transif_file      = data "transif.rem"      (* [iif_automaton] *)
and transiiy_file     = data "transiiy.rem"     (* [iiy_automaton] *)
and transavy_file     = data "transavy.rem"     (* [avy_automaton] *)
and transif2_file     = data "transif2.rem"     (* [iif_automaton] *)
and transiif_file     = data "transiif.rem"     (* [iiif_automaton] *)
and transiv_file      = data "transiv.rem"      (* [iiv_automaton] *)
and transauxi_file    = data "transauxi.rem"    (* [auxi_automaton] *)
and transauxik_file   = data "transauxik.rem"   (* [auxik_automaton] *)
and transauxiick_file = data "transauxiick.rem" (* [auxiick_automaton] *)
and transvoca_file    = data "transvoca.rem"    (* [voca_automaton] *)
and transinv_file     = data "transinv.rem"     (* [inv_automaton] *) 
and transinde_file    = data "transinde.rem"    (* [indeclinable_automaton] *)
and transabsya_file   = data "transabsya.rem"   (* [absolya_automaton] *) 
and transabstvaa_file = data "transabstvaa.rem" (* [absoltvaa_automaton] *) 
and transinftu_file   = data "transinftu.rem"   (* [inftu_automaton] *) 
and transkama_file    = data "transkama.rem"    (* [kama_automaton] *) 
and transsfx_file     = data "transsfx.rem"     (* [sfx_automaton] *) 
and transisfx_file    = data "transisfx.rem"    (* [isfx_automaton] *) 
and transca_file      = data "transca.rem"      (* [cache_automaton] *)
and transstems_file   = data "transstems.rem"   (* [stems_automaton] *)
and declstxt_file     = data "nouns.txt"   (* created by [decline -ascii] *)
and declstex_file     = data "nouns.tex"   (* created by [decline -tex] *)
and declsxml_file     = data "nouns.xml"   (* created by [decline -xml] *)
and rootstxt_file     = data "roots.txt"   (* created by [conjug -ascii] *)
and rootstex_file     = data "roots.tex"   (* created by [conjug -tex] *)
and rootsxml_file     = data "roots.xml"   (* created by [conjug -xml] *)
and partstxt_file     = data "parts.txt"   (* created by [declinep -ascii] *)
and partstex_file     = data "parts.tex"   (* created by [declinep -tex] *)
and partsxml_file     = data "parts.xml"   (* created by [declinep -xml] *)
and mw_exc_file       = data "mw_exceptions.rem" (* for MW indexing *)
and mw_index_file     = data "mw_index.rem"
and guess_auto        = data "guess_index.rem"
;
(* Next are the inflected forms banks, read at cgi time by [Lexer.load_morphs] *)
value public_nouns_file     = public_data "nouns.rem" 
and public_nouns2_file      = public_data "nouns2.rem" 
and public_pronouns_file    = public_data "pronouns.rem" 
and public_preverbs_file    = public_data "preverbs.rem" 
and public_roots_file       = public_data "roots.rem" 
and public_peris_file       = public_data "peris.rem" 
and public_lopas_file       = public_data "lopas.rem" 
and public_lopaks_file      = public_data "lopaks.rem" 
and public_roots_infos_file = public_data "roots_infos.rem" 
and public_parts_file       = public_data "parts.rem" 
and public_partvocs_file    = public_data "partvocs.rem" 
and public_iics_file        = public_data "iics.rem" 
and public_iics2_file       = public_data "iics2.rem" 
and public_piics_file       = public_data "piics.rem" 
and public_ifcs_file        = public_data "ifcs.rem" 
and public_ifcs2_file       = public_data "ifcs2.rem" 
and public_sfxs_file        = public_data "sfxs.rem"  (* taddhita suffix forms *)
and public_isfxs_file       = public_data "isfxs.rem" (* taddhita suffix stems *)
and public_iivs_file        = public_data "iivs.rem" 
and public_avyayais_file    = public_data "avyayais.rem" (* iic stems of avyayiibhava cpds *)
and public_avyayafs_file    = public_data "avyayafs.rem" (* ifc stems of avyayiibhava cpds *)
and public_auxis_file       = public_data "auxi.rem" 
and public_auxiks_file      = public_data "auxik.rem" 
and public_auxiicks_file    = public_data "auxiick.rem" 
and public_iifcs_file       = public_data "iifcs.rem" 
and public_vocas_file       = public_data "voca.rem" 
and public_invs_file        = public_data "invs.rem"
and public_inde_file        = public_data "indecls.rem" 
and public_absya_file       = public_data "absya.rem"  
and public_abstvaa_file     = public_data "abstvaa.rem"  
and public_inftu_file       = public_data "inftu.rem"
and public_kama_file        = public_data "kama.rem" 
and public_stems_file       = public_data "stems.rem"  
and public_roots_usage_file = public_data "roots_usage.rem" 
and public_lexical_kridantas_file = public_data "lexical_kridantas.rem" 
and public_unique_kridantas_file = public_data "unique_kridantas.rem" 
and public_verblinks_file   = public_data "verblinks.rem" 

and public_mw_exc_file = public_data "mw_exceptions.rem"
and public_mw_index_file = public_data "mw_index.rem"
and public_guess_auto = public_data "guess_index.rem"
(* Next segmenting transducers, read at cgi time by [Lexer.load_transducer] *)
and public_transn_file     = public_data "transn.rem" 
and public_transn2_file    = public_data "transn2.rem" 
and public_transpn_file    = public_data "transpn.rem" 
and public_transr_file     = public_data "transr.rem" 
and public_transperi_file  = public_data "transperi.rem" 
and public_translopa_file  = public_data "translopa.rem" 
and public_transp_file     = public_data "transp.rem" 
and public_transpa_file    = public_data "transpa.rem" 
and public_translopak_file = public_data "translopak.rem" 
and public_transpav_file   = public_data "transpav.rem" 
and public_transic_file    = public_data "transic.rem" 
and public_transic2_file   = public_data "transic2.rem" 
and public_transpic_file   = public_data "transpic.rem" 
and public_transif_file    = public_data "transif.rem" 
and public_transif2_file   = public_data "transif2.rem" 
and public_transiiy_file   = public_data "transiiy.rem" 
and public_transavy_file   = public_data "transavy.rem" 
and public_transiif_file   = public_data "transiif.rem" 
and public_transiv_file    = public_data "transiv.rem"
and public_transauxi_file  = public_data "transauxi.rem"
and public_transauxik_file = public_data "transauxik.rem"
and public_transauxiick_file = public_data "transauxiick.rem"
and public_transvoca_file  = public_data "transvoca.rem"
and public_transinv_file   = public_data "transinv.rem" 
and public_transinde_file  = public_data "transinde.rem"
and public_transabsya_file = public_data "transabsya.rem" 
and public_transabstvaa_file = public_data "transabstvaa.rem" 
and public_transinftu_file   = public_data "transinftu.rem" 
and public_transkama_file    = public_data "transkama.rem" 
and public_transsfx_file   = public_data "transsfx.rem" 
and public_transisfx_file  = public_data "transisfx.rem" 
and public_transca_file    = public_data "transca.rem"
and public_transstems_file = public_data "transstems.rem"
and public_sandhis_id_file = public_data "sandhis_id.rem" 
and public_cache_file      = public_data "cache.rem"
and public_cache_txt_file  = public_data "cache.txt"
;
value skt_dir_url = Paths.skt_dir_url
;
(* Relative paths of top directory of site and sub directories *)
value web_dico_url = skt_dir_url ^ "DICO/"
and mw_dico_url    = skt_dir_url ^ "MW/"
and web_images_url = skt_dir_url ^ "IMAGES/" 
and corpus_url     = skt_dir_url ^ "CORPUS/"
and sanskrit_page_url l = skt_dir_url ^ (site_entry_page l)
and faq_page_url l      = skt_dir_url ^ (faq_page l)
and portal_page_url l   = skt_dir_url ^ (portal_page l)
;
(* style sheet built by Css module *)
value style_sheet = "style.css"
;
value css_file = dico_page style_sheet
;
(* javascript to fake dev UTF8 as VH *)
value deva_reader = "utf82VH.js"
;
(* Absolute URLs for cgis *)
value dico_page_url name = web_dico_url ^ name
; 
value style_sheet_url  = dico_page_url style_sheet
and deva_reader_url    = dico_page_url deva_reader
and indexer_page_url l = dico_page_url (dico_index_page l)
and reader_page_url l  = dico_page_url (dico_reader_page l)
and grammar_page_url l = dico_page_url (dico_grammar_page l)
and sandhi_page_url l  = dico_page_url (dico_sandhi_page l)
; 
value image name = web_images_url ^ name
;
value ocaml_logo = image "icon_ocaml.png" 
and inria_logo   = image "logo_inria.png"
and favicon      = image "favicon.ico"
;
value reader_meta_title = title "Sanskrit Reader Companion"
and parser_meta_title = title "Sanskrit Reader Assistant"
and dico_title_fr = h1_title "Dictionnaire H&eacute;ritage du Sanscrit"
and dummy_title_fr = h1_title "Le sanscrit pour les nuls"
and dico_title_en = h1_title (if narrow_screen then "Sanskrit Lexicon" 
                              else "Monier-Williams Dictionary")
and dummy_title_en = h1_title "Sanskrit made easy" 
and stem_title_en = h1_title (if narrow_screen then "Sanskrit Stemmer"
                              else "Search for atomic inflected forms")
and reader_title = h1_title (if narrow_screen then "Sanskrit Reader"
                             else "The Sanskrit Reader Companion")
and parser_title = h1_title (if narrow_screen then "Sanskrit Parser"
                             else "The Sanskrit Parser Assistant")
and graph_meta_title = title "Sanskrit Segmenter Summary"
and user_aid_meta_title = title "User Feedback"
and interface_title = h1_title (if narrow_screen then "Summarizer"
                                else "Sanskrit Segmenter Summary")
and user_aid_title = h1_title (if narrow_screen then "User Feedback"
                            else "Feedback for Unknown Chunks")
;
value dico_title = fun
  [ French  -> dico_title_fr
  | English -> dico_title_en 
  ]
;
(* We set and reset [output_channel] to designate either a static html file
   under creation or [stdout] to produce a cgi output dynamic page.
   This is awful and should be fixed one day.
*)
value output_channel = ref stdout
;
value ps s = output_string output_channel.val s 
and pc c = output_char output_channel.val c 
and pi i = output_string output_channel.val (string_of_int i) 
;
value line () = pc '\n'
and sp () = ps " "
and pl s = ps (s ^ "\n")
; 
value meta_program l = List.iter pl (List.map meta_prefix l)
;
value javascript ref =
  xml_begin_with_att "script" [ ("type","text/javascript"); ("src",ref) ]
 (* Caution - necessary to separate begin and end *) 
 ^ xml_end "script" 
;
(* dyn=True for dynamic pages created by cgis, False for static pages in DICO *)
value deva_read_script dyn = 
  let ref = if dyn then deva_reader_url
                   else deva_reader in
  javascript ref
;
value css_link dyn = 
  let ref = if dyn then style_sheet_url (* dynamic page, absolute URL *)
            else style_sheet (* static page in DICO, relative URL *) in
  xml_empty_with_att "link" [ ("rel","stylesheet"); ("type","text/css"); 
                              ("href",ref); ("media","screen,tv") ]
;
value caml_inside dyn = 
  let logo = if dyn then ocaml_logo else rel_ocaml_logo in
  let ocaml_logo = xml_empty_with_att "img" 
      [ ("src",logo); ("alt","Le chameau Ocaml"); ("height","50") ] in
  anchor_ref ocaml_site ocaml_logo
and inria_inside dyn = (* Inria new logo - clickable *)
  let logo = if dyn then inria_logo else rel_inria_logo in
  let inria_logo = xml_empty_with_att "img" 
      [ ("src",logo); ("alt","Logo Inria"); ("height","50") ] in
  anchor_ref inria_site inria_logo
;
value favicon dyn =
  let path = if dyn then favicon else rel_favicon in
  "<link rel=\"shortcut icon\" href=\"" ^ path ^ "\">" 
; 
value page_begin_dyn dyn title = do
  { pl doctype
  ; ps (xml_begin_with_att "html" [])
  ; pl (xml_begin "head")                           (* ( *)
  ; meta_program contents_instructions              (* . *)
  ; pl title                                        (* . *)
  ; meta_program title_instructions                 (* . *)
  ; pl (css_link dyn)                               (* . *)
  ; pl (favicon dyn)                                (* . *)
  ; pl (deva_read_script dyn) (* devanagari input *)(* . *)
  ; pl (xml_end "head")                             (* ) *)
  }
;
value open_html_file f title = do (* for building the Web services pages *)
  { output_channel.val := open_out f; page_begin_dyn False title } 
;
value page_begin = page_begin_dyn True (* for cgi output page *)
;
value version lang = 
  let lang_str = 
     match lang with 
     [ Some Html.French  -> " (French)"
     | Some Html.English -> " (English)"
     | None -> ""
     ] in
  h3_begin B3 ^ Date.version ^ lang_str ^ h3_end 
;
value print_title lang title = do
  { pl (table_begin Centered)
  ; ps tr_begin
  ; ps th_begin 
  ; pl title
  ; pl (version lang)
  ; ps th_end
  ; ps tr_end
  ; pl table_end
  }
and print_title_solid color lang title = do
  { pl (table_begin (centered color))
  ; ps tr_begin
  ; ps th_begin 
  ; pl title
  ; pl (version lang)
  ; ps th_end
  ; ps tr_end
  ; pl table_end
  }
;
value print_transliteration_help lang = 
  if narrow_screen then () 
  else do
  { ps "Transliteration help "
  ; pl (anchor_ref (rel_faq_page_url lang ^ "#transliteration") "here")
  }
;
value transliteration_switch_default dft id =
  option_select_default_id id "t" 
       [ (" Velthuis ","VH",dft="VH")  (* Default Velthuis *)
       ; ("    WX    ","WX",dft="WX")  (* Infamous WaX from U. Hyderabad *) 
       ; ("    KH    ","KH",dft="KH")  (* Kyoto-Harvard *)
       ; ("   SLP1   ","SL",dft="SL")  (* Sanskrit Library Sloppy 1 *)
       ; ("Devanagari","DN",dft="DN")  (* Devanagari UTF-8 *)
       ; ("   IAST   ","RN",dft="RN")  (* Indological romanisation in UTF-8 *)
       ]
;
value print_transliteration_switch id =
  ps (transliteration_switch_default Paths.default_transliteration id)
;
value print_lexicon_select lexicon = do 
  { ps "Lexicon Access " 
  ; pl (option_select_default "lex" 
         [ ("    Heritage     ","SH","SH"=lexicon)  (* Sanskrit Heritage *)
         ; (" Monier-Williams ","MW","MW"=lexicon)  (* Monier-Williams *)
         ])
  }
;
value print_index_help lang = 
  if narrow_screen then () else do
  { pl (par_begin G2)
  ; pl html_break 
  ; ps "Search for an entry matching an initial pattern:"
  ; pl html_break 
  ; print_transliteration_help lang
  ; pl par_end (* G2 *)
  }
;
value print_dummy_help_en () = 
  if narrow_screen then () else do
  { pl (par_begin G2)
  ; ps "The simplified interface below allows search without diacritics"
  ; pl html_break 
  ; pl "Proper names may be entered with an initial capital"
  ; pl par_end (* G2 *)
  }
;
value print_stemmer_help_en () = 
  if narrow_screen then () else do
  { ps (par_begin G2)
  ; pl "Submit candidate form and category"
  ; pl html_break 
  ; pl "Forms ended in r should not be entered with final visarga"
  ; pl html_break 
  ; pl "Compound words may be recognized with the Reader interface"
  ; pl html_break 
  ; pl par_end (* G2 *)
  }
;
value open_page_with_margin width = 
  let margin = string_of_int width ^ "pt" in 
  let attr = [ noborder; nopadding; ("cellspacing",margin); fullwidth ] in do 
  { pl (table_begin_style (background Chamois) attr)
  ; ps tr_begin (* closed by [close_page_with_margin] *)
  ; pl td_begin
  }
and close_page_with_margin () = do 
  { pl html_break 
  ; ps td_end
  ; ps tr_end
  ; pl table_end
  }
;
value indexer_page l = dico_page (dico_index_page l) (* [mk_index_page]   *) 
and grammar_page l = dico_page (dico_grammar_page l) (* [mk_grammar_page] *) 
and reader_page l = dico_page (dico_reader_page l)   (* [mk_reader_page]  *) 
and sandhi_page l = dico_page (dico_sandhi_page l)   (* [mk_sandhi_page]  *) 
; 

value print_site_map dyn lang = (* the various Web services of the site *)
  if dyn then do 
  { ps (anchor_ref (sanskrit_page_url lang) (emph "Top")); pl " | " 
  ; ps (anchor_ref (indexer_page_url lang) (emph "Index")); pl " | "
  ; ps (anchor_ref (indexer_page_url lang ^ "#stemmer") (emph "Stemmer")); pl " | "
  ; ps (anchor_ref (grammar_page_url lang) (emph "Grammar")); pl " | "
  ; ps (anchor_ref (sandhi_page_url lang) (emph "Sandhi")); pl " | "
  ; ps (anchor_ref (reader_page_url lang) (emph "Reader")); pl " | "
  ; ps (anchor_ref corpus_manager_cgi (emph "Corpus")); pl " | "
  ; ps (anchor_ref (faq_page_url lang) (emph "Help")); pl " | "
  ; pl (anchor_ref (portal_page_url lang) (emph "Portal"))
  }
 else do
  { ps (anchor_ref (rel_sanskrit_page_url lang) (emph "Top")); pl " | "
  ; ps (anchor_ref (dico_index_page lang) (emph "Index")); pl " | "
  ; ps (anchor_ref (dico_index_page lang ^ "#stemmer") (emph "Stemmer")); pl " | "
  ; ps (anchor_ref (dico_grammar_page lang) (emph "Grammar")); pl " | "
  ; ps (anchor_ref (dico_sandhi_page lang) (emph "Sandhi")); pl " | "
  ; ps (anchor_ref (dico_reader_page lang) (emph "Reader")); pl " | "
  ; ps (anchor_ref corpus_manager_cgi (emph "Corpus")); pl " | "
  ; ps (anchor_ref (rel_faq_page_url lang) (emph "Help")); pl " | "
  ; pl (anchor_ref (rel_portal_page_url lang) (emph "Portal"))
  }
;
value pad () = do (* ad-hoc vertical padding to make room for the bandeau *)
  { pl (table_begin Pad60)
  ; ps tr_begin 
  ; ps (xml_begin "td" ^ xml_end "td") 
  ; ps tr_end
  ; pl table_end
  }
;
value print_bandeau_enpied_dyn dyn lang color = do
  { pad () (* necessary padding to avoid hiding by bandeau *)
  ; pl (elt_begin "div" Enpied)
  ; ps (table_begin Bandeau)
  ; ps tr_begin (* main row begin *)
  ; pl td_begin
  ; pl (caml_inside dyn)
  ; ps td_end 
  ; pl td_begin
  ; pl (table_begin Tcenter)
  ; ps tr_begin 
  ; pl td_begin
  ; print_site_map dyn lang
  ; ps td_end
  ; ps tr_end
  ; ps tr_begin 
  ; pl td_begin
  ; ps copyright
  ; ps td_end
  ; ps tr_end   (* copyright row end *)
  ; ps table_end
  ; ps td_end
  ; pl td_begin
  ; pl (inria_inside dyn)
  ; ps html_break 
  ; ps td_end
  ; ps tr_end 
  ; ps table_end (* Bandeau *)
  ; pl (xml_end "div") (* end Enpied *)
  }
;
(* Simputer - legacy code - could be reused for smartphones *)
value print_bandeau_entete color = 
  let margin_bottom height = "margin-bottom:" ^ points height in
  let interval height = do 
    { ps tr_begin
    ; pl (td [ ("width","100%"); ("style",margin_bottom height) ]) 
    ; ps tr_end
    } in do 
  { pl (table_begin_style (background color) 
            [ noborder; nopadding; ("cellspacing","5pt"); fullwidth ])
  ; interval 10
  ; ps tr_begin
  ; pl (xml_begin_with_att "td" [ fullwidth; ("align","center") ])
  ; print_site_map True Html.English
  ; ps td_end
  ; ps tr_end
  ; interval 10
  ; pl table_end
  }
;
value page_end_dyn dyn lang bandeau = do 
  { match Html.target with
    [ Html.Simputer -> ()
    | Html.Computer | Html.Station  | Html.Server
      -> if bandeau then print_bandeau_enpied_dyn dyn lang Cyan else ()
    ] 
  ; pl body_end
  ; pl (xml_end "html")
  }
;
value page_end = page_end_dyn True 
;
value close_html_file lang b = do
  { page_end_dyn False lang b; close_out output_channel.val }
;
value close_html_dico () = close_html_file Html.French True 
;
value http_header = "Content-Type: text/html\n"
;
(* Print the HTTP header only when it is required, i.e. only if it is
   a CGI output.  *)
value maybe_http_header () =
  if output_channel.val = stdout then pl http_header else ()
;
value javascript_tooltip ="wz_tooltip.js"
;
(* This could be any absolute server where Platform is installed *)
(* Maybe should be put back in config? but versioning problem... *)
value remote_server_host = "http://sanskrit.inria.fr/" 
;
(* This toogle controls accessibility of University of Hyderabad tools.
   It is controled by [ML/SCLpaths.ml], which is not part of the git repository,
   and is initialised by default to [SETUP/dummy_SCLpaths.ml] at make time. *)
value scl_toggle =
  not (SCLpaths.scl_url="") (* True if SCL tools are installed *)
;
value interaction_modes_default mode =  
  [ (" Summary ","g",mode="g") 
  ; (" Tagging ","t",mode="t") 
  ; (" Parsing ","p",mode="p") 
  ] @ if scl_toggle then (* Needs the SCL tools *)
  [ (" Analysis ","o",mode="o") ] else []
;
value interaction_modes = 
  interaction_modes_default "g" (* default summary mode *)
;

(* NB Interface and Parser have their own prelude. *)
(* [reader_prelude] is invoked by Parser through Rank and by [Mk_reader_page] *)
value reader_prelude title = do 
  { pl http_header  
  ; page_begin reader_meta_title 
  ; pl (body_begin Chamois_back)
  ; if scl_toggle then (* external call SCL (experimental) *)
       pl (javascript (SCLpaths.scl_url ^ javascript_tooltip))
    else ()
  ; pl title 
  ; open_page_with_margin 15
  }
;
(* cgi invocation *)
value cgi_begin cgi convert = 
  xml_begin_with_att "form" 
    [ ("action",cgi); ("method","get")
    ; ("onsubmit","return " ^ convert ^ "()") ] (* input conversion script *)
  ^ elt_begin "span" Latin12
and cgi_reader_begin cgi convert = (* do not use for pages with multiple cgi *)
  xml_begin_with_att "form" 
    [ ("id","this_form"); ("action",cgi); ("method","get")
    ; ("onsubmit","return " ^ convert ^ "()") ] (* input conversion script *)
  ^ elt_begin "span" Latin12
and cgi_end = xml_end "span" ^ xml_end "form"
;

(* Failsafe aborting of cgi invocation *)
value abort lang s1 s2 = do 
  { pl (table_begin_style (centered Yellow) [ noborder; ("cellspacing","20pt") ])
  ; ps tr_begin
  ; ps th_begin
  ; ps (html_red s1)  (* Report anomaly *)
  ; pl (html_blue s2) (* Optional specific message *)
  ; ps th_end
  ; ps tr_end
  ; pl table_end 
  ; close_page_with_margin ()
  ; page_end lang True
  }
;

(*i end; i*)
