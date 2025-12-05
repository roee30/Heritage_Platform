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
let resources name = Paths.skt_resources_dir ^ (name ^ "/")
  
(* Read-only resources *)
let heritage_dir = resources "DICO"
and data_dir = resources "DATA"
  
(* Contains the locally computed transducers databases *)
let local_data_dir = "DATA/"
  
(* Local resources *)
let top_dev_dir name = Paths.skt_install_dir ^ (name ^ "/")
  
let dico_dir = top_dev_dir "DICO"
  
(* augments local copy of DICO dynamically *)
(* Absolute paths of target server *)
let top_site_dir name = Paths.public_skt_dir ^ (name ^ "/")
  
let public_dico_dir = top_site_dir "DICO"
and (* hypertext dictionary *) public_data_dir = top_site_dir "DATA"
and (* linguistic data for cgis *) corpus_dir = top_site_dir "CORPUS"
  
(* Corpus tree *)
(*i Regression - obsolete
[(* This file is accessible only from Station clients in [var_dir] *)
value var_dir = top_site_dir "VAR" (* Parser dynamic regression suites *)
and regression_file_name = "regression" (* regression analysis stuff *)
; ]i*)
let data name = data_dir ^ name
and local_data name = local_data_dir ^ name
and dico_page name = dico_dir ^ name
and public_data name = public_data_dir ^ name
and public_dico_page name = public_dico_dir ^ name
  
let public_entries_file = public_dico_page "entries.rem"
and (* created by [make releasedata], read by [indexer] *)
  public_dummies_file = public_dico_page "dummies.rem"
  
(* created by [make releasedata], read by [indexerd] *)
let sandhis_file = data "sandhis.rem"
and sandhis_pv_file = data "sandhis_pv.rem"
and sandhis_ph_file = data "sandhis_ph.rem"
and public_sandhis_file = public_data "sandhis.rem"
and public_sandhis_id_file = public_data "sandhis_id.rem"
and automaton_stats = data "automaton.txt"
  
(* text file created by [make_automaton -stats] *)
let nouns_file = data "nouns.rem"
and
  (* created by [make_nouns], read by [Print_inflected.read_nouns],
       used by [Make_transducers.make_transducers] to generate the transducers,
       copied in [public_nouns_file] by make releasecgi for use by cgis *)
  pronouns_file = data "pronouns.rem"
and (* created by [make_nouns], read by [Print_inflected.read_pronouns] *)
  roots_infos_file = data "roots_infos.rem"
and (* created by [Print_dict.postlude], read by [Make_roots.make_roots] *)
  roots_usage_file = data "roots_usage.rem"
and (* created by [Print_html.postlude], read by [Dispatcher.roots_usage] *)
  verblinks_file = data "verblinks.rem"
and (* created by [Print_dict.postlude] calling [Roots.collect_preverbs] *)
  (* read by [Print_html], [Make_preverbs] *)
  (* copied in [public_verblinks_file] *) lexical_kridantas_file =
  data "lexical_kridantas.rem"
and
  (* created by [Print_dict.postlude], read by [Make_roots.roots_to_conjugs] *)
  unique_kridantas_file = data "unique_kridantas.rem"
and (* created by [Make_roots.roots_to_conjugs] *) roots_file =
  data "roots.rem"
and (* created by [make_roots], read by [reader], [tagger] \& [indexer] *)
  peris_file = data "peris.rem"
and lopas_file = data "lopas.rem"
and parts_file = data "parts.rem"
and partvocs_file = data "partvocs.rem"
and lopaks_file = data "lopaks.rem"
and preverbs_file = data "preverbs.rem"
and (* created by [make_preverbs], read by [make_inflected] *)
  preverbs_textfile trans = data (trans ^ "_preverbs.txt")
and (* created by [make_preverbs] for documentation *) iics_file =
  data "iics.rem"
and
  (* created by [make_nouns], copied in [public_iics_file] by make install,
       read by [make_automaton] invoked from DATA/Makefile *)
  iifcs_file = data "iifcs.rem"
and (* iic stems of ifc nouns *) vocas_file = data "voca.rem"
and (* created by [make_nouns] etc. *) vocaf_file = data "vocaf.rem"
and (* created by [make_nouns] etc. *) invs_file = data "invs.rem"
and (* created by [make_nouns] etc. *) piics_file = data "piics.rem"
and (* created by [make_roots] etc. *) ifcs_file = data "ifcs.rem"
and (* created by [make_nouns] etc. *) avyayais_file = data "avyayais.rem"
and (* iic stems of avyayiibhava cpds *) avyayafs_file = data "avyayafs.rem"
and (* ifc stems of avyayiibhava cpds *) iivs_file = data "iivs.rem"
and (* created by [make_roots] etc. *) auxis_file = data "auxi.rem"
and (* created by [make_roots] etc. *) auxiinvs_file = data "auxiinv.rem"
and (* created by [make_roots] etc. *) auxiks_file = data "auxik.rem"
and (* created by [make_roots] etc. *) auxiicks_file = data "auxiick.rem"
and (* created by [make_roots] etc. *) indecls_file = data "indecls.rem"
and (* created by [make_roots] etc. *) indifcs_file = data "indifcs.rem"
and (* created by [make_roots] etc. *) absya_file = data "absya.rem"
and (* created by [make_roots] etc. *) abstvaa_file = data "abstvaa.rem"
and (* created by [make_roots] etc. *) inftu_file = data "inftu.rem"
and (* created by [make_roots] etc. *) kama_file = data "kama.rem"
and (* created by [make_nouns] etc. *)
  (* The transducers file, made by [make_transducers] *) transducers_file 
  = local_data "transducers.rem"
and (* transducers *) mw_exc_file = data "mw_exceptions.rem"
and (* for MW indexing *) mw_index_file = data "mw_index.rem"
and guess_auto = data "guess_index.rem"
  
(* Next are the inflected forms banks, read at cgi time by [Lexer.load_morphs] *)
let public_nouns_file = public_data "nouns.rem"
and public_pronouns_file = public_data "pronouns.rem"
and public_preverbs_file = public_data "preverbs.rem"
and public_roots_file = public_data "roots.rem"
and public_peris_file = public_data "peris.rem"
and public_lopas_file = public_data "lopas.rem"
and public_lopaks_file = public_data "lopaks.rem"
and public_parts_file = public_data "parts.rem"
and public_partvocs_file = public_data "partvocs.rem"
and public_iics_file = public_data "iics.rem"
and public_piics_file = public_data "piics.rem"
and public_ifcs_file = public_data "ifcs.rem"
and public_iivs_file = public_data "iivs.rem"
and public_avyayais_file = public_data "avyayais.rem"
and (* iic avyayiibhavas *) public_avyayafs_file = public_data "avyayafs.rem"
and (* ifc avyayiibhavas *) public_auxis_file = public_data "auxi.rem"
and public_auxiinvs_file = public_data "auxiinv.rem"
and public_auxiks_file = public_data "auxik.rem"
and public_auxiicks_file = public_data "auxiick.rem"
and public_iifcs_file = public_data "iifcs.rem"
and public_vocas_file = public_data "voca.rem"
and public_invs_file = public_data "invs.rem"
and public_inde_file = public_data "indecls.rem"
and public_indifcs_file = public_data "indifcs.rem"
and public_absya_file = public_data "absya.rem"
and public_abstvaa_file = public_data "abstvaa.rem"
and public_inftu_file = public_data "inftu.rem"
and public_kama_file = public_data "kama.rem"
and public_vocaf_file = public_data "vocaf.rem"
and public_stems_file = public_data "stems.rem"
and public_roots_infos_file = public_data "roots_infos.rem"
and public_roots_usage_file = public_data "roots_usage.rem"
and public_lexical_kridantas_file = public_data "lexical_kridantas.rem"
and public_unique_kridantas_file = public_data "unique_kridantas.rem"
and public_verblinks_file = public_data "verblinks.rem"
and public_mw_exc_file = public_data "mw_exceptions.rem"
and public_mw_index_file = public_data "mw_index.rem"
and public_guess_auto = public_data "guess_index.rem"
and (* The segmenting transducers, read at cgi time by [Load_transducers] *)
  public_transducers_file = public_data "transducers.rem"
and (* The cached supplementary nouns dictionary *) public_cache_file 
  = public_data "cache.rem"
and (* cache genders *) public_cachei_file = public_data "cachei.rem"
and (* cache iics *) public_cache_txt_file = public_data "cache.txt"
and (* master cache *) public_trans_cache_file = public_data "transca.rem"
and public_trans_cachei_file = public_data "transcai.rem"
  

