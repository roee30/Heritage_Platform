(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* module Data html = struct *)

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
and corpus_dir        = top_site_dir "CORPUS" (* Corpus tree *)
;
(* Regression deprecated 
[(* This file is accessible only from Station clients in [var_dir] *)
value var_dir = top_site_dir "VAR" (* Parser dynamic regression suites *)
and regression_file_name = "regression" (* regression analysis stuff *)
; ]*)

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
and sandhis_pv_file = public_data "sandhis_pv.rem"  
and sandhis_ph_file = public_data "sandhis_ph.rem"  
and public_sandhis_id_file = public_data "sandhis_id.rem" 
and automaton_stats = data "automaton.txt" 
    (* text file created by [make_automaton -stats] *)
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
and iivs_file     = data "iivs.rem"  (* created by [make_roots] etc. *)
and auxis_file    = data "auxi.rem"  (* created by [make_roots] etc. *)
and auxiks_file   = data "auxik.rem"  (* created by [make_roots] etc. *)
and auxiicks_file = data "auxiick.rem"  (* created by [make_roots] etc. *)
and indecls_file  = data "indecls.rem" (* created by  [make_roots] etc. *)
and absya_file    = data "absya.rem"  (* created by [make_roots] etc. *)
and abstvaa_file  = data "abstvaa.rem"  (* created by [make_roots] etc. *)
and inftu_file    = data "inftu.rem"  (* created by [make_roots] etc. *)
and kama_file     = data "kama.rem"  (* created by [make_nouns] etc. *)

(* Then transducers files, made by [make_transducers] *)
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
and public_trans_cache_file  = public_data "transca.rem"
and public_trans_cachei_file = public_data "transcai.rem"
and public_transstems_file = public_data "transstems.rem"
and public_cache_file      = public_data "cache.rem" (* cache genders *)
and public_cachei_file     = public_data "cachei.rem" (* cache iics *)
and public_cache_txt_file  = public_data "cache.txt" (* master cache *) 
;
