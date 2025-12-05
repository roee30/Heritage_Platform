(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* module Web html = struct *)
(* Module Web reads localisation parameters from paths.ml, 
   created by "make configure" in main directory, called by configure script.
   Describes all installation parameters and resources other than Data. *)
(*  Dynamic html rendering, used by cgis *)
open Html
  
(* French English target Computer etc. *)
(* truncation is the maximum number of solutions computed by the lexer.
   Too small a truncation limit will miss solutions, too large a truncation 
   limit will provoke un unrecoverable choking server failure. This is relevant
   only for the parser (deprecated) mode. The graph interface has no limit. *)
let truncation = 10000
  
(* threshold for printing the list of explicit segmentation solutions *)
let max_count = 100
  
(* do not exceed - rather use the graphical interface *)
let cache_allowed = target = Station
  
(* cache allowed only on Station *)
let cache_active = ref (if cache_allowed then "t" else "f")
  
(* For interface look-and-feel *)
let (display_morph_action, mouse_action_help) =
  match Paths.mouse_action with
  | "CLICK" -> ("onclick", "Click")
  | "OVER" -> ("onMouseover", "Mouse")
  | _ -> failwith "Unknown mouse action, change config file"
  
let cgi_bin name = Paths.cgi_dir_url ^ name
  
(* Call-backs as cgi binaries *)
let index_cgi = cgi_bin Paths.cgi_index
and (* index *) dummy_cgi = cgi_bin Paths.cgi_indexd
and (* index for dummies *) decls_cgi = cgi_bin Paths.cgi_decl
and (* declensions *) conjs_cgi = cgi_bin Paths.cgi_conj
and (* conjugations *) lemmatizer_cgi = cgi_bin Paths.cgi_lemmatizer
and (* lemmatizer *) reader_cgi = cgi_bin Paths.cgi_reader
and (* reader *) parser_cgi = cgi_bin Paths.cgi_parser
and (* parser *) graph_cgi = cgi_bin Paths.cgi_graph
and (* summarizer graphical interface *) user_aid_cgi =
  cgi_bin Paths.cgi_user_aid
and (* unknown chunks processing *) sandhier_cgi = cgi_bin Paths.cgi_sandhier
and (* sandhier *) corpus_manager_cgi = cgi_bin Paths.cgi_corpus_manager
and (* Corpus manager *) save_corpus_cgi = cgi_bin Paths.cgi_save_corpus
and mkdir_corpus_cgi = cgi_bin Paths.cgi_mkdir_corpus
  
let dico_page = Data.dico_page
  
let skt_dir_url = Paths.skt_dir_url
  
(* Relative paths of top directory of site and sub directories *)
let web_dico_url = skt_dir_url ^ "DICO/"
and mw_dico_url = skt_dir_url ^ "MW/"
and web_images_url = skt_dir_url ^ "IMAGES/"
and sanskrit_page_url l = skt_dir_url ^ (site_entry_page l)
and faq_page_url l = skt_dir_url ^ (faq_page l)
and portal_page_url l = skt_dir_url ^ (portal_page l)
  
(* style sheet built by Css module *)
let style_sheet = "style.css"
  
let css_file = dico_page style_sheet
  
(* javascript to fake dev UTF8 as VH *)
let deva_reader = "utf82VH.js"
  
(* Absolute URLs for cgis *)
let dico_page_url name = web_dico_url ^ name
  
let style_sheet_url = dico_page_url style_sheet
and deva_reader_url = dico_page_url deva_reader
and indexer_page_url l = dico_page_url (dico_index_page l)
and reader_page_url l = dico_page_url (dico_reader_page l)
and grammar_page_url l = dico_page_url (dico_grammar_page l)
and sandhi_page_url l = dico_page_url (dico_sandhi_page l)
and corpus_page_url l = dico_page_url (dico_corpus_page l)
  
let image name = web_images_url ^ name
  
let ocaml_logo = image "icon_ocaml.png"
and inria_logo = image "logo_inria.png"
and favicon = image "favicon.ico"
  
let reader_meta_title = title "Sanskrit Reader Companion"
and parser_meta_title = title "Sanskrit Reader Assistant"
and dico_title_fr = h1_title "Dictionnaire H&eacute;ritage du Sanscrit"
and dummy_title_fr = h1_title "Le sanscrit pour les nuls"
and dico_title_en =
  h1_title
    (if narrow_screen
     then "Sanskrit Lexicon"
     else "Monier-Williams Dictionary")
and dummy_title_en = h1_title "Sanskrit made easy"
and stem_title_en =
  h1_title
    (if narrow_screen
     then "Sanskrit Stemmer"
     else "Search for atomic inflected forms")
and reader_title =
  h1_title
    (if narrow_screen
     then "Sanskrit Reader"
     else "The Sanskrit Reader Companion")
and parser_title =
  h1_title
    (if narrow_screen
     then "Sanskrit Parser"
     else "The Sanskrit Parser Assistant")
and graph_meta_title = title "Sanskrit Segmenter Summary"
and user_aid_meta_title = title "User Feedback"
and interface_title =
  h1_title
    (if narrow_screen then "Summarizer" else "Sanskrit Segmenter Summary")
and user_aid_title =
  h1_title
    (if narrow_screen then "User Feedback" else "Feedback for Unknown Chunks")
  
let dico_title =
  function | French -> dico_title_fr | English -> dico_title_en
  
(* We set and reset [output_channel] to designate either a static html file
   under creation or [stdout] to produce a cgi output dynamic page. Ugly. *)
let output_channel = ref stdout
  
let ps s = output_string !output_channel s
and pc c = output_char !output_channel c
and pi i = output_string !output_channel (string_of_int i)
  
let line () = pc '\n'
and sp () = ps " "
and pl s = ps (s ^ "\n")
  
type font = | Deva | Roma

let font_of_string =
  function
  | "deva" -> Deva
  | "roma" -> Roma
  | f -> failwith ("Unknown font " ^ f)
and string_of_font = function | Deva -> "deva" | Roma -> "roma"
  
let default_skt_font = Paths.default_display_font |> font_of_string
  
(* Global communicating the Sanskrit display font to [Morpho_html] *)
let sanskrit_font = ref default_skt_font
  
let toggle_sanskrit_font ft = sanskrit_font := ft
  
let pr_roma code = (* roman with diacritics *)
  ps ((html_red (Canon.uniromcode code)) ^ " ")
and pr_deva code = (* devanagari *)
  ps ((html_devared (Canon.unidevcode code)) ^ " ")
  
let pr_font font word =
  match font with | Deva -> pr_deva word | Roma -> pr_roma word
and pr_i font word =
  ((* special for iic *)
   (match font with
    | Deva -> (pr_deva word; pr_deva [ 0 ])
    | Roma -> (pr_roma word; pr_roma [ 0 ]));
   print_string " ")
  
let meta_program l = List.iter pl (List.map meta_prefix l)
  
let javascript ref =
  (xml_begin_with_att "script" [ ("type", "text/javascript"); ("src", ref) ])
    ^ (* Caution - necessary to separate begin and end *) (xml_end "script")
  
(* dyn=True for dynamic pages created by cgis, False for static pages in DICO *)
let deva_read_script dyn =
  let ref = if dyn then deva_reader_url else deva_reader in javascript ref
  
let js_util_script dyn =
  let js_util_file = "util.js" in
  let prefix = if dyn then dico_page_url else (fun x -> x)
  in javascript (prefix js_util_file)
  
let css_link dyn =
  let ref =
    if dyn
    then style_sheet_url
    else (* dynamic page, absolute URL *) style_sheet
  in
    (* static page in DICO, relative URL *)
    xml_empty_with_att "link"
      [ ("rel", "stylesheet"); ("type", "text/css"); ("href", ref);
        ("media", "screen,tv") ]
  
let caml_inside dyn =
  let logo = if dyn then ocaml_logo else rel_ocaml_logo in
  let ocaml_logo =
    xml_empty_with_att "img"
      [ ("src", logo); ("alt", "Le chameau Ocaml"); ("height", "50") ]
  in anchor_ref ocaml_site ocaml_logo
and inria_inside dyn = (* Inria new logo - clickable *)
  let logo = if dyn then inria_logo else rel_inria_logo in
  let inria_logo =
    xml_empty_with_att "img"
      [ ("src", logo); ("alt", "Logo Inria"); ("height", "50") ]
  in anchor_ref inria_site inria_logo
  
let favicon dyn =
  let path = if dyn then favicon else rel_favicon
  in "<link rel=\"shortcut icon\" href=\"" ^ (path ^ "\">")
  
let page_begin_dyn dyn title =
  (doctype |> pl;
   (xml_begin_with_att "html" []) |> ps;
   (xml_begin "head") |> pl;
   (* ( *)
   meta_program contents_instructions;
   (* . *)
   title |> pl;
   (* . *)
   meta_program title_instructions;
   (* . *)
   (css_link dyn) |> pl;
   (* . *)
   (favicon dyn) |> pl;
   (* . *)
   (deva_read_script dyn) |> pl;
   (* devanagari input *)
   (* . *)
   (js_util_script dyn) |> pl;
   (* . *)
   (xml_end "head") |> pl)
  
(* ) *)
let open_html_file f title =
  ((* for building the Web services pages *)
   output_channel := open_out f;
   page_begin_dyn false title)
  
let page_begin = page_begin_dyn true
  
(* for cgi output page *)
let version lang =
  let lang_str =
    match lang with
    | Some French -> " (French)"
    | Some English -> " (English)"
    | None -> ""
  in (h3_begin B3) ^ (Date.version ^ (lang_str ^ h3_end))
  
let print_title lang title =
  ((table_begin Centered) |> pl;
   tr_begin |> ps;
   th_begin |> ps;
   title |> pl;
   (version lang) |> pl;
   th_end |> ps;
   tr_end |> ps;
   table_end |> pl)
and print_title_solid color lang title =
  ((table_begin (centered color)) |> pl;
   tr_begin |> ps;
   th_begin |> ps;
   title |> pl;
   (version lang) |> pl;
   th_end |> ps;
   tr_end |> ps;
   table_end |> pl)
  
let print_transliteration_help lang =
  if narrow_screen
  then ()
  else
    ("Transliteration help " |> ps;
     (anchor_ref ((rel_faq_page_url lang) ^ "#transliteration") "here") |> pl)
  
let transliteration_switch_default dft id =
  option_select_default_id id "t"
    [ (" Velthuis ", "VH", (dft = "VH")); (* Default Velthuis *)
      ("    WX    ", "WX", (dft = "WX"));
      (* Infamous WaX from U. Hyderabad *)
      ("    KH    ", "KH", (dft = "KH")); (* Kyoto-Harvard *)
      ("   SLP1   ", "SL", (dft = "SL")); (* Sanskrit Library Sloppy 1 *)
      ("Devanagari", "DN", (dft = "DN")); (* Devanagari UTF-8 *)
      ("   IAST   ", "RN", (dft = "RN")) ]
  
(* Indological romanisation in UTF-8 *)
let print_transliteration_switch id =
  (transliteration_switch_default Paths.default_transliteration id) |> pl
  
let print_lexicon_select lexicon =
  ("Lexicon Access " |> ps;
   (option_select_default "lex"
      [ ("    Heritage     ", "SH", ("SH" = lexicon));
        (* Sanskrit Heritage *) (" Monier-Williams ", "MW", ("MW" = lexicon)) ])
     |> (* Monier-Williams *) pl)
  
let print_index_help lang =
  if narrow_screen
  then ()
  else
    ((par_begin G2) |> pl;
     html_break |> pl;
     "Search for an entry matching an initial pattern:" |> ps;
     html_break |> pl;
     print_transliteration_help lang;
     par_end |> pl)
  
(* G2 *)
let print_dummy_help_en () =
  if narrow_screen
  then ()
  else
    ((par_begin G2) |> pl;
     "The simplified interface below allows search without diacritics" |> ps;
     html_break |> pl;
     "Proper names may be entered with an initial capital" |> pl;
     par_end |> pl)
  
(* G2 *)
let print_stemmer_help_en () =
  if narrow_screen
  then ()
  else
    ((par_begin G2) |> ps;
     "Submit candidate form and category" |> pl;
     html_break |> pl;
     "Forms ended in r should not be entered with final visarga" |> pl;
     html_break |> pl;
     "Compound words may be recognized with the Reader interface" |> pl;
     html_break |> pl;
     par_end |> pl)
  
(* G2 *)
let open_page_with_margin width =
  let margin = (string_of_int width) ^ "pt" in
  let attr = [ noborder; nopadding; ("cellspacing", margin); fullwidth ]
  in
    ((table_begin_style (background Chamois) attr) |> pl;
     tr_begin |> ps;
     (* closed by [close_page_with_margin] *)
     td_begin |> pl)
and close_page_with_margin () =
  (html_break |> pl; td_end |> ps; tr_end |> ps; table_end |> pl)
  
let indexer_page l = dico_page (dico_index_page l)
and (* [mk_index_page]   *) grammar_page l = dico_page (dico_grammar_page l)
and (* [mk_grammar_page] *) reader_page l = dico_page (dico_reader_page l)
and (* [mk_reader_page]  *) sandhi_page l = dico_page (dico_sandhi_page l)
and (* [mk_sandhi_page]  *) corpus_page l = dico_page (dico_corpus_page l)
  
(* [mk_corpus_page]  *)
let print_site_map dyn lang = (* the various Web services of the site *)
  if dyn
  then
    ((anchor_ref (sanskrit_page_url lang) (emph "Top")) |> ps;
     " | " |> pl;
     (anchor_ref (indexer_page_url lang) (emph "Index")) |> ps;
     " | " |> pl;
     (anchor_ref ((indexer_page_url lang) ^ "#stemmer") (emph "Stemmer")) |>
       ps;
     " | " |> pl;
     (anchor_ref (grammar_page_url lang) (emph "Grammar")) |> ps;
     " | " |> pl;
     (anchor_ref (sandhi_page_url lang) (emph "Sandhi")) |> ps;
     " | " |> pl;
     (anchor_ref (reader_page_url lang) (emph "Reader")) |> ps;
     " | " |> pl;
     (anchor_ref (corpus_page_url lang) (emph "Corpus")) |> ps;
     " | " |> pl;
     (anchor_ref (faq_page_url lang) (emph "Help")) |> ps;
     " | " |> pl;
     (anchor_ref (portal_page_url lang) (emph "Portal")) |> pl)
  else
    ((anchor_ref (rel_sanskrit_page_url lang) (emph "Top")) |> ps;
     " | " |> pl;
     (anchor_ref (dico_index_page lang) (emph "Index")) |> ps;
     " | " |> pl;
     (anchor_ref ((dico_index_page lang) ^ "#stemmer") (emph "Stemmer")) |>
       ps;
     " | " |> pl;
     (anchor_ref (dico_grammar_page lang) (emph "Grammar")) |> ps;
     " | " |> pl;
     (anchor_ref (dico_sandhi_page lang) (emph "Sandhi")) |> ps;
     " | " |> pl;
     (anchor_ref (dico_reader_page lang) (emph "Reader")) |> ps;
     " | " |> pl;
     (anchor_ref (dico_corpus_page lang) (emph "Corpus")) |> ps;
     " | " |> pl;
     (anchor_ref (rel_faq_page_url lang) (emph "Help")) |> ps;
     " | " |> pl;
     (anchor_ref (rel_portal_page_url lang) (emph "Portal")) |> pl)
  
let pad () =
  ((* ad-hoc vertical padding to make room for the bandeau *)
   (table_begin Pad60) |> pl;
   tr_begin |> ps;
   td_begin |> ps;
   td_end |> ps;
   tr_end |> ps;
   table_end |> pl)
  
let print_bandeau_enpied_dyn dyn lang color =
  (pad ();
   (* necessary padding to avoid hiding by bandeau *)
   (elt_begin "div" Enpied) |> pl;
   (table_begin Bandeau) |> ps;
   tr_begin |> ps;
   (* main row begin *)
   td_begin |> pl;
   (caml_inside dyn) |> pl;
   td_end |> ps;
   td_begin |> pl;
   (table_begin Tcenter) |> pl;
   tr_begin |> ps;
   td_begin |> pl;
   print_site_map dyn lang;
   td_end |> ps;
   tr_end |> ps;
   tr_begin |> ps;
   td_begin |> pl;
   copyright |> ps;
   td_end |> ps;
   tr_end |> ps;
   (* copyright row end *)
   table_end |> ps;
   td_end |> ps;
   td_begin |> pl;
   (inria_inside dyn) |> pl;
   html_break |> ps;
   td_end |> ps;
   tr_end |> ps;
   table_end |> ps;
   (* Bandeau *)
   (xml_end "div") |> pl)
  
(* end Enpied *)
(* Simputer - legacy code - could be reused for smartphones *)
let print_bandeau_entete color =
  let margin_bottom height = "margin-bottom:" ^ (points height) in
  let interval height =
    (tr_begin |> ps;
     (td [ ("width", "100%"); ("style", (margin_bottom height)) ]) |> pl;
     tr_end |> ps)
  in
    ((table_begin_style (background color)
        [ noborder; nopadding; ("cellspacing", "5pt"); fullwidth ])
       |> pl;
     interval 10;
     tr_begin |> ps;
     (xml_begin_with_att "td" [ fullwidth; ("align", "center") ]) |> pl;
     print_site_map true English;
     td_end |> ps;
     tr_end |> ps;
     interval 10;
     table_end |> pl)
  
let page_end_dyn dyn lang bandeau =
  ((match target with
    | Simputer -> ()
    | Computer | Station | Server ->
        if bandeau then print_bandeau_enpied_dyn dyn lang Cyan else ());
   body_end |> pl;
   (xml_end "html") |> pl)
  
let page_end = page_end_dyn true
  
let close_html_file lang b =
  (page_end_dyn false lang b; close_out !output_channel)
  
let close_html_dico () = close_html_file French true
  
let http_header = "Content-Type: text/html\n"
  
(* Print the HTTP header only when it is required, i.e. only if it is
   a CGI output.  *)
let maybe_http_header () =
  if !output_channel = stdout then http_header |> pl else ()
  
let javascript_tooltip = "wz_tooltip.js"
  
(* This could be any absolute server where Platform is installed *)
(* Maybe should be put back in config? but versioning problem... *)
let remote_server_host = "http://sanskrit.inria.fr/"
  
(* This toggle controls accessibility of University of Hyderabad tools.
   It is controled by [ML/SCLpaths.ml], which is not part of the git repository,
   and is initialised by default to [SETUP/dummy_SCLpaths.ml] at make time. *)
let scl_toggle = not (SCLpaths.scl_url = "")
  
(* True if SCL tools are installed *)
let interaction_modes_default mode =
  [ (" Summary ", "g", (mode = "g")); (" Tagging ", "t", (mode = "t"));
    (" Parsing ", "p", (mode = "p")) ] @
    (if scl_toggle
     then (* Needs the SCL tools *) [ (" Analysis ", "o", (mode = "o")) ]
     else [])
  
let interaction_modes = interaction_modes_default "g"
  
(* default graph interface mode *)
let corpus_read_only =
  match target with | Station -> false | Computer | Server | Simputer -> true
  
(* NB Interface and Parser have their own prelude. *)
(* [reader_prelude] is invoked by Parser through Rank and by [Mk_reader_page] *)
let reader_prelude title =
  ((*http_header |> pl ; *)
   page_begin reader_meta_title;
   (body_begin Chamois_back) |> pl;
   if scl_toggle
   then (* external call SCL (experimental) *)
     (javascript (SCLpaths.scl_url ^ javascript_tooltip)) |> pl
   else ();
   title |> pl;
   open_page_with_margin 15)
  
(* cgi invocation *)
let cgi_begin cgi convert =
  (xml_begin_with_att "form"
     [ ("action", cgi); ("method", "get");
       ("onsubmit", ("return " ^ (convert ^ "()"))) ])
    ^ (* input conversion script *) (elt_begin "span" Latin12)
and cgi_reader_begin cgi convert =
  (* do not use for pages with multiple cgi *)
  (xml_begin_with_att "form"
     [ ("id", "this_form"); ("action", cgi); ("method", "get");
       ("onsubmit", ("return " ^ (convert ^ "()"))) ])
    ^ (* input conversion script *) (elt_begin "span" Latin12)
and cgi_end = (xml_end "span") ^ (xml_end "form")
  
(* Failsafe aborting of cgi invocation *)
let abort lang s1 s2 =
  ((table_begin_style (centered Yellow) [ noborder; ("cellspacing", "20pt") ])
     |> pl;
   tr_begin |> ps;
   th_begin |> ps;
   (html_red s1) |> ps;
   (* Report anomaly *)
   (html_blue s2) |> pl;
   (* Optional specific message *)
   th_end |> ps;
   tr_end |> ps;
   table_end |> pl;
   close_page_with_margin ();
   page_end lang true)
  
(* Build an HTML page to report error.  *)
let error_page title_str msg submsg =
  (maybe_http_header ();
   page_begin (title title_str);
   (body_begin Chamois_back) |> pl;
   open_page_with_margin 15;
   (h1_title title_str) |> (print_title (Some default_language));
   abort default_language msg submsg)
  
(* [invalid_corpus_mode_page expected_mode current_mode] generates an HTML on
   [output_channel] to notify the user that the requested operation
   on the corpus is available only in [expected_mode] and not in
   [current_mode].  *)
let invalid_corpus_permission_page expected current =
  error_page "Corpus Manager" "Invalid permission "
    ("Expected permission: " ^
       (expected ^ (" | Current permission: " ^ current)))
  

