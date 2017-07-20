(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                        Gérard Huet & Pawan Goyal                       *)
(*                                                                        *)
(* ©2016 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit Reader Summarizing interface. *)

(* We construct a CGI Interface displaying the segmentation graph in which the 
   user may indicate segments as mandatory checkpoints. At any point he may
   call the standard displaying of all, or of preferred solutions consistent 
   with the current checkpoints. An undo button allows backtracking. *)

open Graph_segmenter; (* [Segment cur_chunk set_cur_offset graph visual] *)
open Phases; (* [Phases] *) 
open Phases; (* [phase is_cache generative] *) 
open Dispatcher; (* [transducer_vect phase Dispatch transition trim_tags] *) 
open Html; 
open Web; (* [ps pl abort reader_cgi scl_toggle] etc. *)
open Cgi; 

module Prel = struct (* Interface's lexer prelude *)

 value prelude () = do
  { maybe_http_header ()
  ; page_begin graph_meta_title 
  ; pl (body_begin Chamois_back)
  ; pl interface_title
  ; pl (h3_begin C3 ^ "Click on " ^ html_green check_sign 
                    ^ " to select segment, click on " ^ html_red x_sign 
                    ^ " to rule out segment" ^ h3_end)
  ; pl (h3_begin C3 ^ mouse_action_help 
                    ^ " on segment to get its lemma" ^ h3_end)
  ; open_page_with_margin 15
  }
;
 end (* Prel *)
;
(* Service routines for morphological query, loading the morphology banks *)

module Lemmas = Load_morphs.Morphs Prel Phases  
;
open Lemmas (* [tags_of morpho] *)
;
open Load_transducers (* [Trans] *)
;
module Transducers = Trans Prel
;
module Machine = Dispatch Transducers Lemmas
;
open Machine (* [cache_phase] *)
;
(* At this point we have a Finite Eilenberg machine ready to instantiate the
Eilenberg component of the Segment module. *)

(* Viccheda sandhi splitting *)

(* Global parameters of the lexer *)
value iterate = ref True (* by default a chunk is a list of words *)
and complete  = ref True (* by default we call the complete segmenter *)
and output_channel = ref stdout (* by default cgi output on standard output *)
;
module Segment_control = struct
 value star = iterate;  (* vaakya vs pada *)
 value full = complete; (* complete vs simplified *)
 value out_chan = output_channel
; 
end (* [Segment_control] *)
;
module Viccheda = Segment Phases Machine Segment_control
;
open Viccheda (* [segment_all visual_width] etc. *)
;
(* At this point we have the sandhi inverser segmenting engine *)

(* Separates tags of homophonous segments vertically *)
value fold_vert f = fold 1 where rec fold n = fun
  [ [] -> () 
  | [ x ] -> f n x
  | [ x :: l ] -> do { f n x; ps html_break; fold (n+1) l }
  ]
;
value print_morph pvs seg_num cached gen form n tag = 
  Morpho_html.print_graph_link pvs cached form (seg_num,n) gen tag 
;
(* tags : Morphology.multitag is the multi-tag of the form of a given phase *)
value print_tags pvs seg_num phase form tags =
  let gen = generative phase 
  and cached = is_cache phase in 
  let ok_tags = if pvs = [] then tags 
                else trim_tags (generative phase) form (Canon.decode pvs) tags
  (* NB Existence of the segment warrants that [ok_tags] is not empty *)
  and ptag = print_morph pvs seg_num cached gen form in 
  fold_vert ptag ok_tags  
;
(*i EXPERIMENTAL: taddhitaantas (ad-hoc) i*)
value print_morph_tad pvs seg_num cache gen stem sfx n tag = 
  Morpho_html.print_graph_link_tad pvs cache stem sfx (seg_num,n) gen tag  
; 
value print_tags_tad pvs seg_num phase stem sfx sfx_tags =  
  let ptag = print_morph_tad pvs seg_num False (generative phase) stem sfx in 
  fold_vert ptag sfx_tags 
;
(* This is called "printing morphology interface style". Taddhitaanta forms
   are printed as fake compounds of iic the stem and ifc the taddhita form. *)
value print_morpho phase word = 
     match tags_of phase word with 
        [ Atomic tags -> print_tags [] 0 phase word tags 
        | Preverbed (_,phase) pvs form tags -> print_tags pvs 0 phase form tags
        | Taddhita (ph,form) sfx _ sfx_tags -> 
            match tags_of ph form with 
            [ Atomic _ -> (* stem, tagged as iic *)
              print_tags_tad [] 0 ph form sfx sfx_tags 
            | Preverbed _ pvs _ _ -> (* stem, tagged as iic *)
              print_tags_tad pvs 0 ph form sfx sfx_tags 
            | _ -> raise (Control.Anomaly "taddhita recursion")
            ]
        ]
    (* PB: if form has homonymy, we get t1 t2 t for [t1 | t2].t - confusion *)
;

(* Parsing mandatory checkpoints *)
open Checkpoints; (* [string_points] *) 

value rpc = Paths.remote_server_host 
and remote = ref False (* local invocation of cgi by default 
                          (switched on to [True] by "abs" cgi parameter) *)
;
value invoke cgi = if remote.val then rpc ^ cgi else cgi 
;
value mem_cpts ind phase_pada =  memrec where rec memrec = fun
  [ [] -> False
  | [ (k,pw,_) :: rest ] -> (k=ind && pw = phase_pada) || memrec rest
  ]
;
value unanalysed (phase,_) = (phase=Phases.unknown)
;
value already_checked = html_blue check_sign
(*i TODO: call to undo this specific checkpoint i*)
;
value call_back text cpts (k,seg) conflict = 
  if mem_cpts k seg cpts then already_checked 
  else if not conflict && not (unanalysed seg) then already_checked
  else let choices b = string_points [ (k,seg,b) :: cpts ] 
       and (out_cgi,sign,color) = 
           if unanalysed seg then (user_aid_cgi,spade_sign,Red_) 
                             else (graph_cgi,   check_sign,Green_) in
       let cgi_select = out_cgi ^ "?" ^ text ^ ";cpts=" ^ (choices True)
       and cgi_reject = out_cgi ^ "?" ^ text ^ ";cpts=" ^ (choices False) in
       anchor color (invoke cgi_select) sign ^ 
          if unanalysed seg then "" else anchor Red_ (invoke cgi_reject) x_sign
;
value call_reader text cpts mode = (* mode = "o", "p", "n" or "t" *)
  let cgi = reader_cgi ^ "?" ^ text ^ ";mode=" ^ mode ^ 
            ";cpts=" ^ string_points cpts in 
  anchor Green_ (invoke cgi) check_sign
;
value call_parser text cpts =
  let cgi = parser_cgi ^ "?" ^ text ^ ";mode=p" ^ 
            ";cpts=" ^ string_points cpts ^ ";n=1" in
  anchor Green_ (invoke cgi) check_sign
;
value call_SL text cpts mode corpus solutions sent_id link_num = 
  let cgi = tomcat ^ corpus ^ "/SaveTagging?slp1Sentence=" 
            ^ text ^ "&numSolutions=" ^ (string_of_int solutions) 
            ^ "&submit=submit&command=resend&sentenceNumber=" ^ sent_id 
            ^ "&linkNumber=" ^ link_num ^ "&displayEncoding=roman&"
            ^ "inflectionFormat=SL&inputEncoding=slp1&OS=MacOS&cpts=" 
            ^ string_points cpts in
  anchor Green_ (invoke cgi) check_sign
;
value sort_check cpts = 
  let compare_index (a,_,_) (b,_,_) = compare a b in
  List.sort compare_index cpts
;
value seg_length = fun
 [ [ -2 :: rest ] -> Word.length rest 
 | w -> Word.length w
 ]
;
value rec merge_rec lpw = fun 
  [ [] -> lpw
  | [ (p, lw) :: rest ] -> merge_rec (fill p lpw lw) rest
       where rec fill p lpw = fun
         [ [] -> lpw
         | [ wh :: rest1 ] -> fill p [ (p,wh) :: lpw ] rest1
         ]
  ] 
;
value build_visual k segments = 
  if segments = [] then () else 
  let phw = merge_rec [] segments in
  let comp_length (_,(a,_)) (_,(b,_)) = compare (seg_length a) (seg_length b) in
  let sorted_seg = List.rev (List.sort comp_length phw) in
  ass_rec sorted_seg
    where rec ass_rec seg =
      let start_ind = find_ind_rec 0
          where rec find_ind_rec n = 
          if k < visual_width.(n) then find_ind_rec (n+1) else n in 
      match seg with
      [ [] -> ()
      | [ (phase,(w1,tr)) :: rest ] -> match phase with 
           [ Phases.Pv | Phases.Pvk | Phases.Pvkc | Phases.Pvkv -> 
             failwith "Preverb in build_visual"
           | _ -> do
             { visual.(start_ind) := visual.(start_ind) @ [ (w1,tr,phase,k) ]
             ; visual_width.(start_ind) := (seg_length w1) + k
             ; ass_rec rest
             }
           ]
      ]
;
(* We check whether the current segment [(w,tr,phase,k)] is conflicting with 
   others at previous offset [l]; if not it is mandatory and marked blue. *)
(* Warning: hairy code, do not change without understanding the theory.  *)
value is_conflicting (w,tr,phase,k) = 
 let l_w = seg_length w in is_conflicting_rec 0
 where rec is_conflicting_rec l = match visual.(l) with 
 [ [] -> False
 | segs -> does_conflict segs
     where rec does_conflict = fun
       [ [] -> is_conflicting_rec (l+1) 
       | [ (w1,tr1,phase1,k1) :: rest ] ->
           if (w1,tr1,phase1,k1)=(w,tr,phase,k) 
           then (* skip itself *) does_conflict rest
           else let l_w1 = seg_length w1 in
                if (k1<=k && k1+l_w1-1>k) 
                || (k1<=k && k1+l_w1-1>=k && l_w=1)
      (* This condition is necessary for the overlapping case *)
                || (k<=k1 && k+l_w-1>k1 && l_w1>1) then 
      (* This condition refines [(k<=k1 && k+l_w-1>k1)] but is modified
         here to take care of cases such as elayati. We do not say that elayati 
         (at k) conflicts with a segment aa (at the same offset). If it were 
         conflicting, there would have existed another segment, which would be 
         sufficient to prove the conflict. It also points to the fact that 
         conflicting is not a symmetric relation. We might have to include 
         a test as we did below *) 
                   if k+l_w-1=k1 then match_tr tr
      (* This is to check for the overlapping case, occurs when [k=k1, l_w=1]. 
         We need to check the sandhi conditions to decide whether this is a case 
         of overlap or conflict. *)
                       where rec match_tr = fun
                         [ [] -> True
                         | [ v :: rst ] -> match v with
                             [ [] -> match_tr rst
                             | _  -> if Word.prefix v (Word.mirror w1) 
                                        then does_conflict rest
                                     else match_tr rst
                             ]
                         ]
                   else if (k1<=k && k1+l_w1-1>=k && l_w=1) then match_tr1 tr1
      (* For the case with [l_w=1], this is to check whether w is the only 
         possible v for w1, then it is an overlap returning a blue sign. 
         If w1 has any other possible v's, there is a conflict. *)
                           where rec match_tr1 = fun
                             [ [] -> does_conflict rest
                             | [ v :: rst ] -> Word.prefix v w || match_tr1 rst
                             ]
                        else True
                   else does_conflict rest
       ]
 ]
;
value rec find_conflict_seg acc l = fun
  [ [] -> List.rev acc
  | [ (w1,tr,phase,k) :: rest ] ->
      let conflict = is_conflicting (w1,tr,phase,k) in
      let seg_here = (w1,phase,k,conflict) in 
      find_conflict_seg [ seg_here :: acc ] l rest
  ]
;
value rec find_conflict l = match visual.(l) with
  [ [] -> ()
  | segs -> do 
    { visual_conf.(l) := find_conflict_seg [] l segs
    ; find_conflict (succ l) 
    }
  ]
;
value make_visual n = vrec 0 
  where rec vrec k = do
    { build_visual k graph.(k)
    ; if k = n-1 then () else vrec (succ k)
    } 
;
value rec print_extra = fun 
  [ 0 -> ()
  | l -> do { ps (td_wrap ""); print_extra (l-1) }
  ]
and fixed_space = td_wrap "&nbsp;"
;
value rec print_first_server chunk = 
  match Word.length chunk with
  [ 0 -> ps fixed_space
  | l -> match chunk with 
         [ [] -> ps fixed_space
         | [ st :: rest ] -> let to_print = Canon.uniromcode [ st ] in do
             { ps (td_wrap to_print)
             ; print_first_server rest
             }
         ]
  ]
;
value call_back_pseudo text cpts ph newpt =
  if List.mem newpt cpts then already_checked 
  else let list_points = [ newpt :: cpts ] in
       let out_cgi = user_aid_cgi in
       let cgi = out_cgi ^ "?" ^ text ^ ";cpts=" ^ (string_points list_points) in
       anchor_pseudo (invoke cgi) ph
;
value un_analyzable (chunk:Word.word) = (Phases.Unknown,Word.mirror chunk)
;
value rec print_first text cpts chunk_orig chunk chunk_ind = 
  match Word.length chunk with
  [ 0 -> ps fixed_space
  | l -> match chunk with 
         [ [] -> ps fixed_space
         | [ st :: rest ] -> let to_print = Canon.uniromcode [ st ] in do
             { let unknown_chunk = (chunk_ind,un_analyzable chunk_orig,True) in
               ps (td_wrap (call_back_pseudo text cpts to_print unknown_chunk))
             ; print_first text cpts chunk_orig rest chunk_ind
             }
         ]
  ]
 ;
(* Making use of the index for printing the chunk callback *)
value rec print_all text cpts chunks index = match chunks with
  [ [] -> ()
  | [ chunk :: rest ] -> do
      { print_first text cpts chunk chunk index
      ; print_all text cpts rest (succ (Word.length chunk))
      }
  ]
;
value print_word last_ind text cpts (rword,phase,k,conflict) = 
  let word = Word.mirror rword in do
  { let extra_space = k-last_ind in 
    if extra_space > 0 then print_extra extra_space else ()
(*i ZZ following not implementable with a fixed css -- not HTML5 compliant i*)
  ; ps (td_begin_att [ ("colspan",string_of_int (seg_length word))
                     ; ("align","left") 
                     ])
  ; let back = background (color_of_phase phase) in 
    pl (table_begin back)
  ; ps tr_begin
  ; ps ("<td " ^ display_morph_action ^ "=\"showBox('")
  ; print_morpho phase word
  ; let close_box = 
        "<a href=&quot;javascript:hideBox()&quot;> " ^ x_sign ^ "</a>', '" in 
    ps (close_box ^ rgb (color_of_phase phase) ^ "', this, event)\">")
  ; Morpho_html.print_final rword (* visarga correction *)
  ; ps td_end
  ; ps tr_end
  ; ps table_end
  ; ps (call_back text cpts (k,(phase,rword)) conflict)
  ; ps td_end
  }
;
value max_col = ref 0
;
value print_row text cpts =  print_this text cpts 0 
  where rec print_this text cpts last_ind = fun 
  [ [] -> let adjust = max_col.val - last_ind in
          if adjust > 0 then print_extra adjust else ()
  | [ (word,phase,k,conflict) :: rest ] -> do
      { print_word last_ind text cpts (word,phase,k,conflict)
      ; print_this text cpts (k + seg_length word) rest
      }
  ]
;
value print_interf text cpts () = vgrec 0 
  where rec vgrec k = 
  match visual_width.(k) with
  [ 0 -> ()
  | _ -> do
    { ps tr_begin
    ; print_row text cpts visual_conf.(k)
    ; pl tr_end
    ; vgrec (succ k)
    }
  ]
;
value update_col_length chunk = 
  max_col.val := succ (max_col.val + Word.length chunk)
;
value invoke_SL text cpts corpus_id count sent_id link_num =
  ps (td_wrap (call_SL text cpts "t" corpus_id count sent_id link_num 
               ^ "Sanskrit Library Interface"))
;
value update_text_with_sol text count = text ^ ";allSol=" ^ match count with
  [ Num.Int n -> string_of_int n
  | _ -> "2147483648" (* [2^31] *)
  ]
;
value call_undo text cpts  = 
  let string_pts = match cpts with 
      [ [] -> "" (* Could raise warning "undo stack empty" *)
      | [ _ :: rest ] -> string_points rest
      ] in
  let cgi = graph_cgi ^ "?" ^ text ^ ";cpts=" ^ string_pts in
  anchor Green_ (invoke cgi) check_sign
;
(* The main procedure for computing the graph segmentation structure *)
value check_sentence translit us text_orig checkpoints sentence 
             (* finally SL corpus links: *) sol_num corpus sent_id link_num = 
  let encode = Encode.switch_code translit in
  let chunker = if us (* sandhi undone *) then Sanskrit.read_raw_sanskrit 
                else (* blanks non-significant *) Sanskrit.read_sanskrit in
  let chunks = chunker encode sentence in 
  let devachunks = List.map Canon.unidevcode chunks in
  let devainput = String.concat " " devachunks  
  and cpts = sort_check checkpoints in 
  let _ = chkpts.all_checks := cpts
  and (flag,count) = segment_all chunks in 
  let text = match sol_num with
             [ "0" -> update_text_with_sol text_orig count
             | _ -> text_orig
             ] in do
  { make_visual cur_chunk.offset
  ; find_conflict 0
  ; pl html_break 
  ; pl (html_latin16 "Sentence: ")
  ; ps (deva16_blue devainput) (* devanagari *)
  ; pl html_break
  ; ps (div_begin Latin16)
  ; pl (table_begin Spacing20)
  ; pl tr_begin
  ; ps (td_wrap (call_undo text checkpoints ^ "Undo"))
  ; let call_scl_parser n = (* invocation of scl parser *)
        if scl_toggle then
           ps (td_wrap (call_reader text cpts "o" ^ "UoH Analysis Mode"))
        else () (* [scl_parser] is not visible unless toggle is set *) in
    match count with 
    [ Num.Int n -> if n > max_count then 
                      (* too many solutions would choke the parsers *) 
                      ps (td_wrap ("(" ^ string_of_int n ^ " Solutions)"))
                   else if n=1 (* Unique remaining solution *) then do
                       { ps (td_wrap (call_parser text cpts ^ "Unique Solution"))
                       ; call_scl_parser 1
                       }
                   else do
        { ps (td_wrap (call_reader text cpts "p" ^ "Filtered Solutions"))
        ; let info = string_of_int n ^ if flag then "" else " Partial" in 
          ps (td_wrap (call_reader text cpts "t" ^ "All " ^ info ^ " Solutions"))
        ; call_scl_parser n
        } 
    | _ -> ps (td_wrap "(More than 2^32 Solutions!)")
    ]
  ; pl tr_end
  ; pl table_end
  ; ps div_end (* Latin16 *)
  ; pl html_break 
  ; ps (div_begin Latin12)
  ; pl (table_begin Tcenter)
  ; ps tr_begin 
  ; List.iter update_col_length chunks 
  ; if Paths.platform="Station" then print_all text checkpoints chunks 0
                                else List.iter print_first_server chunks
  ; pl tr_end
  ; print_interf text checkpoints ()
  ; pl table_end
  ; ps div_end (* Latin12 *)
  ; pl html_break  
  ; reset_graph () 
  ; reset_visual ()
  ; set_cur_offset 0
  ; chkpts.segment_checks := []
  ; max_col.val := 0
  }
;
value arguments trans lex cache st us cp input topic abs sol_num corpus id ln
                corpus_dir sentence_no =
  "t=" ^ trans ^ ";lex=" ^ lex ^ ";cache=" ^ cache ^ ";st=" ^ st ^ ";us=" ^ us ^
  ";cp=" ^ cp ^ ";text=" ^ input ^ ";topic=" ^ topic ^ ";abs=" ^ abs ^ 
  match sol_num with
    [ "0" -> ""
    | n -> ";allSol=" ^ n
    ] ^
  match corpus with
    [ "" -> ""
    | c -> ";corpus=" ^ c ^ ";sentenceNumber=" ^ id ^ ";linkNumber=" ^ ln
    ] ^
  ";" ^ Params.corpus_dir ^ "=" ^ corpus_dir ^
  ";" ^ Params.sentence_no ^ "=" ^ sentence_no
;

(* Cache management *)
value make_cache_transducer (cache : Morphology.inflected_map) = 
  let deco_cache = Mini.minimize (Deco.forget_deco cache) in
  let auto_cache = Automaton.compile Deco.empty deco_cache in do
  { Gen.dump cache public_cache_file (* for [Load_morphs] *)
  ; Gen.dump auto_cache public_transca_file (* for [Load_transducers] *)
  }
;
(* We fill gendered entries incrementally in a [public_cache_txt_file] *)
value append_cache entry gender =    
  let cho = open_out_gen [ Open_wronly; Open_append; Open_text ] 0o777 
                         public_cache_txt_file in do
  { output_string cho ("[{" ^ entry ^ "}] ({" ^ gender  ^ "})\n")
  ; close_out cho
  }
;
value save_sentence_button query =
  Html.center_begin ^
  Web.cgi_begin Web.save_corpus_cgi "" ^
  Html.hidden_input Save_corpus_params.state (Html.escape query) ^
  Html.submit_input "Save sentence" ^
  Web.cgi_end ^
  Html.center_end
;
value continue_reading_button corpdir sentno =
  Html.center_begin ^
  Web.cgi_begin (Cgi.url Web.corpus_manager_cgi ~fragment:sentno) "" ^
  Html.hidden_input Params.corpus_dir corpdir ^
  Html.submit_input "Continue reading" ^
  Web.cgi_end ^
  Html.center_end
;
(* Main body of graph segmenter cgi *)
value graph_engine () = do
  { Prel.prelude () 
  ; let query = Sys.getenv "QUERY_STRING" in
    let env = create_env query in
    let url_encoded_input = get "text" env ""
    and url_encoded_topic = get "topic" env "" (* topic carry-over *)
    and st = get "st" env "t" (* sentence parse default *)
    and cp = get "cp" env "t" (* complete mode default *)
    and us = get "us" env "f" (* sandhied text default *)
    and translit = get "t" env Paths.default_transliteration (* translit input *)
    and lex = get "lex" env Paths.default_lexicon (* lexicon choice *)
    and cache = get "cache" env "f" (* no cache default *) in
    let () = cache_active.val := cache 
    and abs = get "abs" env "f" (* default local paths *) in 
    let lang = language_of lex (* language default *)
    and input = decode_url url_encoded_input (* unnormalized string *)
    and uns = us="t" (* unsandhied vs sandhied corpus *) 
    and () = if st="f" then iterate.val:=False else () (* word stemmer? *)
    and () = if cp="f" then complete.val:=False else () (* simplified reader? *) 
    and () = toggle_lexicon lex 
    and corpus = get "corpus" env ""
    and sent_id = get "sentenceNumber" env "0" 
    and link_num = get "linkNumber" env "0" (* is there a better default? *)
    and sol_num = get "allSol" env "0" in (* Needed for Validate mode *)
    let corpus_dir = Cgi.get Params.corpus_dir env "" in
    let sentence_no = Cgi.get Params.sentence_no env "" in
    let text = arguments translit lex cache st us cp url_encoded_input
                         url_encoded_topic abs sol_num corpus sent_id link_num
                         corpus_dir sentence_no
    and checkpoints = 
      try let url_encoded_cpts = List.assoc "cpts" env in (* do not use get *)
          parse_cpts (decode_url url_encoded_cpts)
      with [ Not_found -> [] ]
    and guess_morph = decode_url (get "guess" env "") 
    and pseudo_gender = decode_url (get "gender" env "") in 
    let _ = if String.length guess_morph > 0 && Paths.platform="Station" then
               let (entry,gender) = match pseudo_gender with 
                                    [ "" -> parse_guess guess_morph 
                                    | g -> (guess_morph,g) 
                                    ] in do
               { append_cache entry gender
               ; let cache_txt_file = Web.public_cache_txt_file in
                 let cache = Nouns.extract_current_cache cache_txt_file in
                  make_cache_transducer cache
               }
            else () in
    let revised = decode_url (get "revised" env "")
    and rev_off = int_of_string (get "rev_off" env "-1")
    and rev_ind = int_of_string (get "rev_ind" env "-1") in
   try do
   { match (revised,rev_off,rev_ind) with
     [ ("",-1,-1) -> check_sentence translit uns text checkpoints 
                                    input sol_num corpus sent_id link_num
     | (new_word,word_off,chunk_ind) -> 
       let chunks = Sanskrit.read_sanskrit (Encode.switch_code translit) input in
       let rec decoded init ind = fun
           [ [] -> String.sub init 0 ((String.length init)-1)
           | [ a :: rest ] -> 
               let ind' = ind+1 
               and init' = if ind = chunk_ind then init ^ new_word ^ "+"
                           else init ^ Canon.switch_decode translit a ^ "+" in
               decoded init' ind' rest
           ] in
       let updated_input = decoded "" 1 chunks in
       let rec find_word_len cur_ind = fun 
           [ [] -> 0
           | [ a :: rest ] -> if cur_ind = chunk_ind then Word.length a
                              else find_word_len (cur_ind+1) rest
           ] in
       let word_len = find_word_len 1 chunks in
       let new_chunk_len = Word.length (Encode.switch_code translit revised) in
       let diff = new_chunk_len - word_len in
       let revised_check = 
         let revise (k,sec,sel) = 
             (if k<word_off then k else k+diff,sec,sel) in
         List.map revise checkpoints
       and updated_text = arguments translit lex cache st us cp updated_input
                            url_encoded_topic abs sol_num corpus sent_id link_num
                            corpus_dir sentence_no
       and new_input = decode_url updated_input in
       check_sentence translit uns updated_text revised_check 
                                  new_input sol_num corpus sent_id link_num
     ]
     (* automatically refreshing the page only if guess parameter *)
   ; if String.length guess_morph > 0 then 
        ps ("<script>\nwindow.onload = function () {window.location=\"" ^
            graph_cgi ^ "?" ^ text ^  
            ";cpts=" ^ (string_points checkpoints) ^ "\";}\n</script>")
     else ()

     (* Save sentence button *)
   ; if Web.corpus_manager_mode corpus_dir sentence_no then
       save_sentence_button query |> Web.pl
     else
       ()

   ; Html.html_break |> Web.pl

     (* Continue reading button *)
   ; if Web.corpus_mode corpus_dir sentence_no then
       continue_reading_button
         (Cgi.decode_url corpus_dir) (Cgi.decode_url sentence_no) |> Web.pl
     else
       ()

   ; close_page_with_margin ()
   ; page_end lang True
   }
   with 
 [ Sys_error s         -> abort lang Control.sys_err_mess s (* file pb *)
 | Stream.Error s      -> abort lang Control.stream_err_mess s (* file pb *)
 | Encode.In_error s   -> abort lang "Wrong input " s
 | Exit (* Sanskrit *) -> abort lang "Wrong character in input" "" 
 | Overflow            -> abort lang "Maximum input size exceeded" ""
 | Invalid_argument s  -> abort lang Control.fatal_err_mess s (* sub *)
 | Failure s           -> abort lang Control.fatal_err_mess s (* anomaly *)
 | End_of_file         -> abort lang Control.fatal_err_mess "EOF" (* EOF *)
 | Not_found           -> let s = "You must choose a parsing option" in
                          abort lang "Unset button in form - " s
 | Control.Fatal s     -> abort lang Control.fatal_err_mess s (* anomaly *)
 | Control.Anomaly s   -> abort lang Control.anomaly_err_mess s
 | _                   -> abort lang Control.fatal_err_mess "Unexpected anomaly" 
 ]
 }
; 
value safe_engine () =
  (* Problem: in case of error, we lose the current language of the session *)
  let abor = abort default_language in
  try graph_engine () with  
  [ Failure s -> abor Control.fatal_err_mess s (* [parse_cpts phase_string] ? *)
  | _ -> abor Control.fatal_err_mess "Unexpected anomaly - broken session" 
  ]
;
