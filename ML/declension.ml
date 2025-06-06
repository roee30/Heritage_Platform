(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI-bin declension for computing declensions.                          *)
(* This CGI is triggered by page [grammar_page] in [dico_dir].            *)
(* Reads its input in shell variable [QUERY_STRING] URI-encoded.          *)
(* Prints an html document of substantive declinations on [stdout].       *)
(*i Test with (csh): setenv QUERY_STRING "q=yoga&g=Mas"; declension      i*)
(*i Web invocation: http://skt_server_url/cgi-bin/sktdeclin?q=e&g=g      i*)

(*i module Declension = struct i*)

open Skt_morph;
open Morphology; (* [Noun_form] etc. *)
open Html;
open Web; (* ps pl etc. *)
open Cgi;
open Multilingual; (* [font Deva Roma compound_name avyaya_name] *)

value dtitle font = h1_title (declension_title narrow_screen font)
and meta_title = title "Sanskrit Grammarian Declension Engine"
and back_ground = background Chamois
and hyperlink_title font link =
  if narrow_screen then link
  else declension_caption font ^ " " ^ link
;

value pr code =
  ps (html_red (Canon.uniromcode code)) (* roman with diacritics *)
and pr_deva code =
  ps (html_devared (Canon.unidevcode code)) (* devanagari *)
;
value pr_f font word = 
  let code = Morpho_html.final word (* visarga correction *) in do
  { match font with
    [ Deva -> pr_deva code
    | Roma -> pr code
    ]
  ; ps " "
  }
and pr_i font word = do (* special for iic *)
  { match font with
    [ Deva -> do { pr_deva word; pr_deva [ 0 ] }
    | Roma -> do { pr word; pr [ 0 ] }
    ]
  ; ps " "
  }
;
value prlist_font font = 
  let pr = pr_f font 
  and bar = html_green " | " in
  prlistrec 
     where rec prlistrec = fun
       [ [] -> ()
       | [ x ] -> pr x
       | [ x :: l ] -> do { pr x; ps bar; prlistrec l }
       ]
;
value display_title font = do
  { pl html_paragraph
  ; pl (table_begin (centered Mauve))
  ; ps tr_begin
  ; ps th_begin
  ; ps (dtitle font)
  ; ps th_end 
  ; ps tr_end 
  ; pl table_end (* Mauve *)
  ; pl html_paragraph
  }
and display_subtitle title = do
  { pl html_paragraph
  ; pl (table_begin (centered Deep_sky))
  ; ps tr_begin
  ; ps th_begin
  ; ps title
  ; ps th_end 
  ; ps tr_end 
  ; pl table_end (* Centered *)
  ; pl html_paragraph
  }
;
value cases_of decls =
  let reorg (v,n,a,i,d,ab,g,l) (c,form) = match c with 
      [ Voc -> ([ form :: v ],n,a,i,d,ab,g,l)
      | Nom -> (v,[ form :: n ],a,i,d,ab,g,l)
      | Acc -> (v,n,[ form :: a ],i,d,ab,g,l)
      | Ins -> (v,n,a,[ form :: i ],d,ab,g,l)
      | Dat -> (v,n,a,i,[ form :: d ],ab,g,l)
      | Abl -> (v,n,a,i,d,[ form :: ab ],g,l)
      | Gen -> (v,n,a,i,d,ab,[ form :: g ],l)
      | Loc -> (v,n,a,i,d,ab,g,[ form :: l ])
      ]
  and init = ([],[],[],[],[],[],[],[])
  in List.fold_left reorg init decls (* (v,n,a,i,d,ab,g,l) *)
;
value print_ro1 caption s d p = do
  { ps tr_begin
  ; ps th_begin
  ; ps caption
  ; ps (xml_next "th") 
  ; ps s
  ; ps (xml_next "th") 
  ; ps d
  ; ps (xml_next "th") 
  ; ps p
  ; ps th_end
  ; pl tr_end
  }
;
value print_row_font font case s d p = 
  let prlist = prlist_font font in do
  { ps (tr_mouse_begin (color Light_blue) (color Pale_yellow))
  ; ps th_begin 
  ; ps case
  ; ps (xml_next "th") 
  ; prlist s
  ; ps (xml_next "th") 
  ; prlist d
  ; ps (xml_next "th") 
  ; prlist p
  ; ps th_end
  ; pl tr_end
  }
;
value display_gender font gender = fun
  [ [] -> ()
  | l -> 
    let reorg (sg,du,pl) (n,c,form) = match n with
        [ Singular -> ([ (c,form) :: sg ],du,pl)
        | Dual     -> (sg,[ (c,form) :: du ],pl)
        | Plural   -> (sg,du,[ (c,form) :: pl ])
        ]
    and init = ([],[],[]) in
    let (s,d,p) = List.fold_left reorg init l in
    let (v1,n1,a1,i1,d1,b1,g1,l1) = cases_of s
    and (v2,n2,a2,i2,d2,b2,g2,l2) = cases_of d
    and (v3,n3,a3,i3,d3,b3,g3,l3) = cases_of p
    and caption = gender_caption gender font 
    and print_row = print_row_font font in do
    { pl html_paragraph
    ; pl (table_begin_style Inflexion [ ("border","2"); padding5 ])
    ; let sing = number_caption Singular font
      and dual = number_caption Dual font 
      and plur = number_caption Plural font in
      print_ro1 caption sing dual plur 
    ; print_row (case_caption Nom font) n1 n2 n3
    ; print_row (case_caption Voc font) v1 v2 v3
    ; print_row (case_caption Acc font) a1 a2 a3
    ; print_row (case_caption Ins font) i1 i2 i3
    ; print_row (case_caption Dat font) d1 d2 d3
    ; print_row (case_caption Abl font) b1 b2 b3
    ; print_row (case_caption Gen font) g1 g2 g3
    ; print_row (case_caption Loc font) l1 l2 l3
    ; ps table_end
    ; pl html_paragraph
    }
  ]
;
value display_iic font = fun
  [ [] -> ()
  | l -> do 
    { pl html_paragraph
    ; ps (h3_begin C3)
    ; ps (compound_name font); ps " "
    ; let print_iic w = pr_i font w in
      List.iter print_iic l
    ; ps h3_end
    }
  ]
;
value display_avy font = fun
  [ [] -> ()
  | l -> do 
    { pl html_paragraph
    ; ps (h3_begin C3)
    ; ps (avyaya_name font); ps " "
    ; let ifc_form w = [ 0 ] (* - *) @ w in
      let print_iic w = pr_f font (ifc_form w) in
      List.iter print_iic l
    ; ps h3_end
    }
  ]
;
value sort_out accu form = fun
     [ [ (_,morphs) ] -> List.fold_left (reorg form) accu morphs
      where reorg f (mas,fem,neu,any,iic,avy) = fun
        [ Noun_form g n c -> let t = (n,c,f) in 
            match g with 
              [ Mas -> ([ t :: mas ],fem,neu,any,iic,avy)
              | Fem -> (mas,[ t :: fem ],neu,any,iic,avy)
              | Neu -> (mas,fem,[ t :: neu ],any,iic,avy)
              | Deictic _ -> (mas,fem,neu,[ t :: any ],iic,avy)
              ]
        | Bare_stem | Auxi_form -> (mas,fem,neu,any,[ f :: iic ],avy)
        | Avyayaf_form -> (mas,fem,neu,any,iic,[ f :: avy ])
        | Ind_form _ | Verb_form _ _ _  | Ind_verb _ | Abs_root _ 
        | Avyayai_form | Unanalysed | PV _ 
        | Part_form _ _ _ _ ->
          failwith "Unexpected form in declensions"
        ]
     | _ -> failwith "Weird table"
     ]
and init = ([],[],[],[],[],[])
;
value display_inflected font (gen_deco,pn_deco,voca_deco,iic_deco,avy_deco) = 
  let nouns = Deco.fold sort_out init gen_deco in 
  let non_vocas = Deco.fold sort_out nouns pn_deco in 
  let (mas,fem,neu,any,_,_) = Deco.fold sort_out non_vocas voca_deco 
  and iic = List.map fst (Deco.contents iic_deco) 
  and avy = List.map fst (Deco.contents avy_deco) in do
  { pl center_begin
  ; display_gender font Mas mas 
  ; display_gender font Fem fem 
  ; display_gender font Neu neu
  ; display_gender font (Deictic Numeral) any (* arbitrary *)
  ; display_iic font iic 
  ; display_avy font avy 
  ; pl center_end 
  ; pl html_paragraph
  }
;
(* [entry:skt] [part:string] *)
value emit_decls font entry decli part = 
  let inflected = Nouns.fake_compute_decls (entry,decli) part in
  display_inflected font inflected 
;
value look_up font entry decli part = 
  let code = Encode.code_string entry in (* normalisation *)
  let e = Canon.decode code in (* coercion skt to string *)
  emit_decls font e decli part
;
(* This is very fragile: lexicon update induces code adaptation. *)
(* Temporary - should be subsumed by unique naming structure. *)
value resolve_homonym stem = match stem with 
  [ "atra" | "ad" | "abhii" | "iiz" | ".rc" | "chad" | "dam" | "dah" | "daa" 
  | "diz" | "diiv" | "duh" | "d.rz" | "druh" | "dvi.s" | "dhii" | "nas" | "nii" 
  | "pad" | "budh" | "bhii" | "bhuu" | "math" | "yaa" | "yuj" | "raa" | "raaj" 
  | "luu" | "viraaj" | "viz" | "vii" | "zubh" | "sa" | "sah" | "saa" | "s.rj" 
  | "snih" | "snuh" | "han" 
    -> stem ^ "#2"
  | "agha" | "afga" | "aja" | "aaza" | "e.sa"  | "ka" | "kara" | "tapas"
  | "dhaavat" | "nimita" | "pa" | "bhavat" | "bhaama" | "ya" | "yama"
  | "yaat.r" (* 1/2 *) | "vaasa" | "zaava" | "zrava.na" | "zvan" | "sthaa"
    -> stem ^ "#1"
  | "paa" -> stem ^ "#3"
  | _ -> stem
  ] 
;
value in_lexicon entry = (* entry as a string in VH transliteration *)
  Index.is_in_lexicon (Encode.code_string entry)
and doubt s = "?" ^ s
;
value gender_of = fun
  [ "Mas" -> Mas
  | "Fem" -> Fem
  | "Neu" -> Neu
  | "Any" -> Deictic Numeral (* arbitrary *)
  | s -> failwith ("Weird gender" ^ s)
  ] 
;
value decls_engine () = do
  { pl http_header
  ; page_begin meta_title 
  ; pl (body_begin back_ground)
  ; let query = Sys.getenv "QUERY_STRING" in
    let env = create_env query in 
    let url_encoded_entry = try List.assoc "q" env 
                            with [ Not_found -> failwith "Entry name missing" ]
    and url_encoded_gender = get "g" env "Mas"
    and url_encoded_participle = get "p" env ""
    and url_encoded_source = get "r" env ""
        (* optional root origin - used by participles in conjugation tables *)
    and font = font_of_string (get "font" env Paths.default_display_font)
(*  and stamp = get "v" env "" - Obsolete *)
    and translit = get "t" env "VH" (* DICO created in VH trans *)
    and lex = get "lex" env "SH" (* default Heritage *) in 
    let entry_tr = decode_url url_encoded_entry (* : string in translit *)
    and gender = gender_of (decode_url url_encoded_gender)
    and part = decode_url url_encoded_participle
    and code = Encode.switch_code translit
    and lang = language_of lex 
    and source = decode_url url_encoded_source (* cascading from conjug *)
    and () = toggle_lexicon lex in
    try do 
      { (* if lex="MW" then () else if stamp=Install.stamp then ()
           else raise (Control.Anomaly "Corrupt lexicon"); - OBS *)
        display_title font
      ; let word = code entry_tr in
        let entry_VH = Canon.decode word in (* ugly detour via VH string *) 
                       (* will be avoided by unique name lookup *) 
        let entry = resolve_homonym entry_VH in (* compute homonymy index *)
        let link =
          if in_lexicon entry then Morpho_html.skt_anchor False font entry
             (* We should check it is indeed a substantive entry 
                and that Any is used for deictics/numbers (TODO) *)
          else let root = if source = "" then "?" (* unknown in lexicon *)
                          else " from " ^ 
               if in_lexicon source then Morpho_html.skt_anchor False font source
               else doubt (Morpho_html.skt_roma source) in
               Morpho_html.skt_roma entry ^ root in
        let subtitle = hyperlink_title font link in do
        { display_subtitle (h1_center subtitle)
        ; try look_up font entry (Nouns.Gender gender) part
          with [ Stream.Error s -> failwith s ] 
        }
      ; page_end lang True
      }
    with [ Stream.Error _ -> 
             abort lang ("Illegal " ^ translit ^ " transliteration ") entry_tr ]
  }
;
value safe_engine () =
  let abor = abort default_language in
  try decls_engine () with 
  [ Sys_error s        -> abor Control.sys_err_mess s (* file pb *)
  | Stream.Error s     -> abor Control.stream_err_mess s (* file pb *)
  | Invalid_argument s -> abor Control.fatal_err_mess s (* sub *)
  | Failure s          -> abor Control.fatal_err_mess s (* anomaly *)
  | Control.Fatal s    -> abor Control.fatal_err_mess s (* anomaly *)
  | Not_found          -> abor Control.fatal_err_mess "assoc" (* assoc *)
  | Control.Anomaly s  -> abor Control.fatal_err_mess ("Anomaly: " ^ s)
  | Nouns.Report s     -> abor "Gender anomaly - " s 
  | End_of_file        -> abor Control.fatal_err_mess "EOF" (* EOF *)
  | Encode.In_error s  -> abor "Wrong_input " s
  | Exit               -> abor "Wrong character in input - " "use ASCII" (* Sanskrit *)
  | _                  -> abor Control.fatal_err_mess "anomaly" (* ? *)
  ]
;
safe_engine ()
;

(*i end; i*)
