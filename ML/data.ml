(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* module Data html = struct *)

(* Absolute paths on development site *)
value resources name = Paths.skt_resources_dir ^ name ^ "/"
;
(* Read-only resources *)
value heritage_dir  = resources "DICO" 
and data_dir        = resources "DATA" 
;
(* Contains the locally computed transducers databases *)
value local_data_dir  = "DATA/" 
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
and corpus_dir        = top_site_dir "CORPUS" (* Corpus tree *)
;
(*i Regression - obsolete
[(* This file is accessible only from Station clients in [var_dir] *)
value var_dir = top_site_dir "VAR" (* Parser dynamic regression suites *)
and regression_file_name = "regression" (* regression analysis stuff *)
; ]i*)

value data name = data_dir ^ name
and local_data name = local_data_dir ^ name
and dico_page name = dico_dir ^ name
and public_data name = public_data_dir ^ name
and public_dico_page name = public_dico_dir ^ name
;
value public_entries_file = public_dico_page "entries.rem" 
(* created by [make releasedata], read by [indexer] *)
and public_dummies_file = public_dico_page "dummies.rem" 
(* created by [make releasedata], read by [indexerd] *)
;
value sandhis_file = data "sandhis.rem"  
and sandhis_pv_file = data "sandhis_pv.rem"  
and sandhis_ph_file = data "sandhis_ph.rem"  
and public_sandhis_file = public_data "sandhis.rem"  
and public_sandhis_id_file = public_data "sandhis_id.rem" 
and automaton_stats = data "automaton.txt" 
    (* text file created by [make_automaton -stats] *)
;                              
value nouns_file = data "nouns.rem"  
    (* created by [make_nouns], read by [Print_inflected.read_nouns],
       used by [Make_transducers.make_transducers] to generate the transducers,
       copied in [public_nouns_file] by make releasecgi for use by cgis *)
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
    (* created by [Print_dict.postlude], read by [Make_roots.roots_to_conjugs] *)
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
and iifcs_file    = data "iifcs.rem" (* iic stems of ifc nouns *)
and vocas_file    = data "voca.rem"  (* created by [make_nouns] etc. *)
and vocaf_file    = data "vocaf.rem"  (* created by [make_nouns] etc. *)
and invs_file     = data "invs.rem"  (* created by [make_nouns] etc. *)
and piics_file    = data "piics.rem" (* created by [make_roots] etc. *)
and ifcs_file     = data "ifcs.rem"  (* created by [make_nouns] etc. *)
and avyayais_file = data "avyayais.rem" (* iic stems of avyayiibhava cpds *)
and avyayafs_file = data "avyayafs.rem" (* ifc stems of avyayiibhava cpds *)
and iivs_file     = data "iivs.rem"  (* created by [make_roots] etc. *)
and auxis_file    = data "auxi.rem"  (* created by [make_roots] etc. *)
and auxiinvs_file = data "auxiinv.rem" (* created by [make_roots] etc. *)
and auxiks_file   = data "auxik.rem"   (* created by [make_roots] etc. *)
and auxiicks_file = data "auxiick.rem" (* created by [make_roots] etc. *)
and indecls_file  = data "indecls.rem" (* created by [make_roots] etc. *)
and indifcs_file  = data "indifcs.rem" (* created by [make_roots] etc. *)
and absya_file    = data "absya.rem"   (* created by [make_roots] etc. *)
and abstvaa_file  = data "abstvaa.rem" (* created by [make_roots] etc. *)
and inftu_file    = data "inftu.rem"   (* created by [make_roots] etc. *)
and kama_file     = data "kama.rem"    (* created by [make_nouns] etc. *)

(* The transducers file, made by [make_transducers] *)
and transducers_file   = local_data "transducers.rem"  (* transducers *)

and mw_exc_file       = data "mw_exceptions.rem" (* for MW indexing *)
and mw_index_file     = data "mw_index.rem"
and guess_auto        = data "guess_index.rem"
and comp_freq_txt_file    = data "comp_freq.tsv"
and pada_freq_txt_file    = data "pada_freq.tsv"
and word_freq_txt_file    = data "word_freq.tsv"
and comp_trans_freq_txt_file    = data "comp_trans_freq.tsv"
and pada_trans_freq_txt_file    = data "pada_trans_freq.tsv"
and comp_freq_file    = data "comp_freq.rem"
and pada_freq_file    = data "pada_freq.rem"
and word_freq_file    = data "word_freq.rem"
and comp_trans_freq_file    = data "comp_trans_freq.rem"
and pada_trans_freq_file    = data "pada_trans_freq.rem"
and comp_morphs_freq_file    = data "comp_morph_freq.rem"
and pada_morphs_freq_file    = data "pada_morph_freq.rem"
;
(* Next are the inflected forms banks, read at cgi time by [Lexer.load_morphs] *)
value public_nouns_file     = public_data "nouns.rem" 
and public_pronouns_file    = public_data "pronouns.rem" 
and public_preverbs_file    = public_data "preverbs.rem" 
and public_roots_file       = public_data "roots.rem" 
and public_peris_file       = public_data "peris.rem" 
and public_lopas_file       = public_data "lopas.rem" 
and public_lopaks_file      = public_data "lopaks.rem" 
and public_parts_file       = public_data "parts.rem" 
and public_partvocs_file    = public_data "partvocs.rem" 
and public_iics_file        = public_data "iics.rem" 
and public_piics_file       = public_data "piics.rem" 
and public_ifcs_file        = public_data "ifcs.rem" 
and public_iivs_file        = public_data "iivs.rem" 
and public_avyayais_file    = public_data "avyayais.rem" (* iic avyayiibhavas *)
and public_avyayafs_file    = public_data "avyayafs.rem" (* ifc avyayiibhavas *)
and public_auxis_file       = public_data "auxi.rem" 
and public_auxiinvs_file    = public_data "auxiinv.rem" 
and public_auxiks_file      = public_data "auxik.rem" 
and public_auxiicks_file    = public_data "auxiick.rem" 
and public_iifcs_file       = public_data "iifcs.rem" 
and public_vocas_file       = public_data "voca.rem" 
and public_invs_file        = public_data "invs.rem"
and public_inde_file        = public_data "indecls.rem" 
and public_indifcs_file     = public_data "indifcs.rem" 
and public_absya_file       = public_data "absya.rem"  
and public_abstvaa_file     = public_data "abstvaa.rem"  
and public_inftu_file       = public_data "inftu.rem"
and public_kama_file        = public_data "kama.rem" 
and public_vocaf_file       = public_data "vocaf.rem" 
and public_stems_file       = public_data "stems.rem"  
and public_roots_infos_file = public_data "roots_infos.rem" 
and public_roots_usage_file = public_data "roots_usage.rem" 
and public_lexical_kridantas_file = public_data "lexical_kridantas.rem" 
and public_unique_kridantas_file  = public_data "unique_kridantas.rem" 
and public_verblinks_file   = public_data "verblinks.rem" 
and public_mw_exc_file = public_data "mw_exceptions.rem"
and public_mw_index_file = public_data "mw_index.rem"
and public_guess_auto = public_data "guess_index.rem"
(* The segmenting transducers, read at cgi time by [Load_transducers] *)
and public_transducers_file  = public_data "transducers.rem" 
(* The cached supplementary nouns dictionary *)
and public_cache_file      = public_data "cache.rem" (* cache genders *)
and public_cachei_file     = public_data "cachei.rem" (* cache iics *)
and public_cache_txt_file  = public_data "cache.txt" (* master cache *) 
and public_trans_cache_file  = public_data "transca.rem"
and public_trans_cachei_file = public_data "transcai.rem"
and public_comp_freq_txt_file    = public_data "comp_freq.tsv"
and public_pada_freq_txt_file    = public_data "pada_freq.tsv"
and public_word_freq_txt_file    = public_data "word_freq.tsv"
and public_comp_trans_freq_txt_file    = public_data "comp_trans_freq.tsv"
and public_pada_trans_freq_txt_file    = public_data "pada_trans_freq.tsv"
and public_comp_freq_file    = public_data "comp_freq.rem"
and public_pada_freq_file    = public_data "pada_freq.rem"
and public_word_freq_file    = public_data "word_freq.rem"
and public_comp_trans_freq_file    = public_data "comp_trans_freq.rem"
and public_pada_trans_freq_file    = public_data "pada_trans_freq.rem"
and public_comp_morphs_freq_file    = public_data "comp_morph_freq.rem"
and public_pada_morphs_freq_file    = public_data "pada_morph_freq.rem"
;
