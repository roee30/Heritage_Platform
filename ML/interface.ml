(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                        Gérard Huet & Pawan Goyal                       *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit Reader Summarizing interface. Yields sktgraph.cgi *)

(* We construct a CGI Interface displaying the segmentation graph in which the 
   user may indicate segments as mandatory checkpoints. At any point he may
   call the standard displaying of all, or of preferred solutions consistent 
   with the current checkpoints. An undo button allows backtracking. *)

module Interface = struct

open Graph_segmenter; (* [Segment cur_chunk set_cur_offset graph visual] *)
open Phases; (* [Phases] *) 
open Phases; (* [phase is_cache generative] *) 
open Dispatcher; (* [transducer_vect phase Dispatch transition trim_tags] *) 
open Html; (* html constructors *)
open Web; (* [ps pl abort reader_cgi scl_toggle] etc. *) 
open Cgi; (* [url get decode_url] *)

module Prel = struct (* Interface's lexer prelude *)

 value prelude () = do
  { pl http_header
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
open Machine 
;
(* At this point we have a Finite Eilenberg machine ready to instantiate *)
(* the Eilenberg component of the Segment module.                        *)

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
open Viccheda (* [segment_iter visual_width] etc. *)
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
(* This is called "printing morphology interface style". *)
value print_morpho phase word = 
     match tags_of phase word with 
        [ Atomic tags -> print_tags [] 0 phase word tags 
        | Preverbed (_,phase) pvs form tags -> print_tags pvs 0 phase form tags
        ]
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
       let call_back flag = out_cgi ^ "?" ^ text ^ ";cpts=" ^ choices flag in
       let cgi_select = call_back True
       and cgi_reject = call_back False in
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
(*i Legacy interface with Sanskrit Library [
value call_SL text cpts mode corpus solutions sent_id link_num = 
  let cgi = tomcat ^ corpus ^ "/SaveTagging?slp1Sentence=" 
            ^ text ^ "&numSolutions=" ^ (string_of_int solutions) 
            ^ "&submit=submit&command=resend&sentenceNumber=" ^ sent_id 
            ^ "&linkNumber=" ^ link_num ^ "&displayEncoding=roman&"
            ^ "inflectionFormat=SL&inputEncoding=slp1&OS=MacOS&cpts=" 
            ^ string_points cpts in
  anchor Green_ (invoke cgi) check_sign
;
value invoke_SL text cpts corpus_id count sent_id link_num =
  ps (td_wrap (call_SL text cpts "t" corpus_id count sent_id link_num 
               ^ "Sanskrit Library Interface"))
;] i*)
value sort_check cpts = 
  let compare_index (a,_,_) (b,_,_) = compare a b in
  List.sort compare_index cpts
;
value seg_length = fun
 [ [ -2 :: rest ] -> Word.length rest (* lopa does not count *)
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
           [ Phases.Pv | Phases.Pvkc | Phases.Pvkv -> 
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
   others at previous offset [n]; if not it is mandatory and marked blue. *)
(* Returns True for blue mandatory segments, False for green/red optional ones *)
(* Warning: very hairy code, do not change without understanding the theory.  *)
value is_conflicting ((w,tr,ph,k) as segment) =
 let l = seg_length w in is_conflicting_rec 0
 where rec is_conflicting_rec n = (* n is position in input string *)
 match visual.(n) with 
 [ [] -> False (* will exit here when n is length of input *) 
 | segs -> does_conflict segs (* we search for conflicting segments *) 
     where rec does_conflict = fun 
       [ [] -> is_conflicting_rec (n+1) (* go to next input position *)
       | [ ((w',tr',ph',k') as segment') :: rest ] -> 
           if segment'=segment then (* skip itself *) does_conflict rest
           else let l' = seg_length w' in
                if (k'<=k && k'+l'-1>k) (* w inside w' *)
                || (k'<=k && k'+l'-1>=k && l=1) (* w is a or aa *)
                   (* This condition is necessary for the overlapping case *)
                || (k<=k' && k+l-1>k') then 
                   if k+l-1=k' then let r' = Word.mirror w' in match_tr tr
      (* This is to check for the overlapping case, occurs when [k=k', l=1]. 
         We need to check the sandhi conditions to decide whether this is a case 
         of overlap or conflict. *)
                       where rec match_tr = fun
                         [ [] -> True
                         | [ v :: rst ] -> match v with
                             [ [] -> match_tr rst
                             | _  -> if Word.prefix v r' 
                                        then does_conflict rest
                                     else match_tr rst
                             ]
                         ]
                   else if (k'<=k && k'+l'-1=k && l=1) then match_tr' tr'
      (* For the case with [l=1], this is to check whether w is the only 
         possible v for w', in which case it is an overlap returning a blue sign.
         If w' has any other possible v's, there is a conflict. *)
      (* This may only occur if w=[1] (a) and w' ends in a or aa *)       
      (* NB. In naabhaava.h caakiirti.h a should be marked blue 
         but in mahaajana.h after checking mahaa a should not be marked blue *)
                           where  match_tr' = fun
                             [ [ v ] -> not (v = w) || does_conflict rest
                             | _ -> True
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
value update_text_with_sol text count = text ^ ";allSol=" ^ string_of_int count
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
  and (full,count) = segment_iter chunks in (* full iff all chunks segment *)
  let text = match sol_num with
             [ "0" -> update_text_with_sol text_orig count
             | _ -> text_orig
             ] in do
  {make_visual cur_chunk.offset
  ; find_conflict 0
  ; html_break |> pl
  ; html_latin16 "Sentence: " |> pl
  ; deva16_blue devainput |> ps (* devanagari *)
  ; html_break |> ps
  ; div_begin Latin16 |> ps
  ; table_begin Spacing20 |> pl
  ; tr_begin |> pl (* tr begin *)
  ; td_wrap (call_undo text checkpoints ^ "Undo") |> ps
  ; let call_scl_parser n = (* invocation of scl parser *)
        if scl_toggle then
           td_wrap (call_reader text cpts "o" ^ "UoH Analysis Mode") |> ps
        else () (* [scl_parser] is not visible unless toggle is set *) in
    if count > max_count then 
       (* too many solutions would choke the parsers *) 
       td_wrap ("(" ^ string_of_int count ^ " Solutions)") |> ps
    else if count=1 (* Unique remaining solution *) then do
            { td_wrap (call_parser text cpts ^ "Unique Solution") |> ps
            ; call_scl_parser 1
            }
         else do
       { td_wrap (call_reader text cpts "p" ^ "Filtered Solutions") |> ps
       ; let info = string_of_int count ^ if full then "" else " Partial" in 
         td_wrap (call_reader text cpts "t" ^ "All " ^ info ^ " Solutions") |> ps
       ; call_scl_parser count
       } 
  ; tr_end |> pl   (* tr end *)
  ; table_end |> pl
  ; div_end |> ps (* Latin16 *)
  ; html_break |> pl
  ; div_begin Latin12 |> ps
  ; table_begin Tcenter |> pl
  ; tr_begin |> ps
  ; List.iter update_col_length chunks 
  ; if Paths.platform="Station" then print_all text checkpoints chunks 0
                                else List.iter print_first_server chunks
  ; tr_end |> pl
  ; print_interf text checkpoints ()
  ; table_end |> pl
  ; div_end |> ps (* Latin12 *)
  ; html_break |> pl
  }
;
value arguments trans lex cache st us cp input topic abs sol_num corpus id ln
                corpus_permission corpus_dir sentence_no =
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
  ";" ^ Params.corpus_permission ^ "=" ^ corpus_permission ^
  ";" ^ Params.corpus_dir ^ "=" ^ corpus_dir ^
  ";" ^ Params.sentence_no ^ "=" ^ sentence_no
;

(* Cache management *)
(* [ (Morphology.inflected_map * Morphology.inflected_map) -> unit] *)
value make_cache_transducers (cache,cachei) =
  let deco_cache = Mini.minimize (Deco.forget_deco cache) 
  and deco_cachei = Mini.minimize (Deco.forget_deco cachei) in
  let auto_cache = Automaton.compile Deco.empty deco_cache 
  and auto_cachei = Automaton.compile Deco.empty deco_cachei in do
  { Gen.dump cache public_cache_file (* for [Load_morphs] *)
  ; Gen.dump cachei public_cachei_file (* id *)
  ; Gen.dump auto_cache public_trans_cache_file (* for [Load_transducers] *)
  ; Gen.dump auto_cachei public_trans_cachei_file (* id *)
  }
;
(* We fill gendered entries incrementally in [public_cache_txt_file] *)
value append_cache entry gender =    
  let cho = open_out_gen [ Open_wronly; Open_append; Open_text ] 0o777 
                         public_cache_txt_file in do
  { output_string cho ("[{" ^ entry ^ "}] ({" ^ gender  ^ "})\n")
  ; close_out cho
  }
;
value save_button query nb_sols =
  center_begin ^
  cgi_begin save_corpus_cgi "" ^
  hidden_input Save_corpus_params.state (escape query) ^
  hidden_input Save_corpus_params.nb_sols (nb_sols |> string_of_int |> escape) ^
  submit_input "Save" ^ 
  cgi_end ^
  center_end
;
value quit_button corpmode corpdir sentno =
  let submit_button_label = Web_corpus.(
    match corpmode with
    [ Annotator -> "Abort"
    | Reader | Manager -> "Continue reading"
    ])
  and permission = Web_corpus.string_of_permission corpmode in
  center_begin ^
      cgi_begin (url corpus_manager_cgi ~fragment:sentno) "" ^
           hidden_input Params.corpus_dir corpdir ^
           hidden_input Params.corpus_permission permission ^
           submit_input submit_button_label ^
      cgi_end ^
  center_end
;
(* Main body of graph segmenter cgi *)
value graph_engine () = do
  { Prel.prelude () 
  ; let query = Sys.getenv "QUERY_STRING" in
    let env = create_env query in
    (* Multiple environment variables according to modes of use are: 
       text topic st cp us t lex cache abs cpts (standard mode) 
       allSol (deprecated Validate mode)
       corpus sentenceNumber linkNumber (Corpus mode)
       corpdir sentno corpmode (defined in Params) 
       guess gender revised [rev_off] [rev_ind] (User-aid) *)
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
    and () = toggle_lexicon lex (* sticky lexicon switch *)
    and corpus = get "corpus" env "" 
    and sent_id = get "sentenceNumber" env "0" 
    and link_num = get "linkNumber" env "0" (* is there a better default? *)
    and sol_num = get "allSol" env "0" in (* Needed for Validate mode *)
    let url_enc_corpus_permission = (* Corpus mode *)
        get Params.corpus_permission env "true" in
    let corpus_permission = 
      url_enc_corpus_permission
      |> decode_url
      |> Web_corpus.permission_of_string in
    let corpus_dir = get Params.corpus_dir env "" in
    let sentence_no = get Params.sentence_no env "" in
    let text = arguments translit lex cache st us cp url_encoded_input
                         url_encoded_topic abs sol_num corpus sent_id link_num
                         url_enc_corpus_permission corpus_dir sentence_no
    and checkpoints = 
      try let url_encoded_cpts = List.assoc "cpts" env in (* do not use get *)
          parse_cpts (decode_url url_encoded_cpts)
      with [ Not_found -> [] ]
    and guess_morph = decode_url (get "guess" env "") (* User-aid guessing *)
    and pseudo_gender = decode_url (get "gender" env "") in 
    let _ = if String.length guess_morph > 0 && Paths.platform="Station" then
               (* User-aid cache acquisition *)
               let (entry,gender) = match pseudo_gender with 
                                    [ "" -> parse_guess guess_morph 
                                    | g -> (guess_morph,g) 
                                    ] in do
               { append_cache entry gender
               ; let cache_txt_file = public_cache_txt_file in
                 let caches = Nouns.extract_current_caches cache_txt_file in
                 make_cache_transducers caches
               }
            else () in
    let revised = decode_url (get "revised" env "") (* User-aid revision *)
    and rev_off = int_of_string (get "rev_off" env "-1") 
    and rev_ind = int_of_string (get "rev_ind" env "-1") in 
   try do
   { match (revised,rev_off,rev_ind) with
     [ ("",-1,-1) -> (* Standard input processing *** Main call *** *)
       check_sentence translit uns text checkpoints input sol_num
                      corpus sent_id link_num
     | (new_word,word_off,chunk_ind) (* User-aid revision mode *) -> 
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
       let word_len = find_word_len 1 chunks 
       and new_chunk_len = Word.length (Encode.switch_code translit revised) in
       let diff = new_chunk_len-word_len in
       let revised_check = 
         let revise (k,sec,sel) = (if k<word_off then k else k+diff,sec,sel) in
         List.map revise checkpoints
       and updated_text = arguments translit lex cache st us cp updated_input
                            url_encoded_topic abs sol_num corpus sent_id link_num
                            url_enc_corpus_permission corpus_dir sentence_no
       and new_input = decode_url updated_input in
       check_sentence translit uns updated_text revised_check 
                      new_input sol_num corpus sent_id link_num
     ]
     (* Rest of the code concerns Corpus mode *)
     (* automatically refreshing the page only if guess parameter *)
   ; if String.length guess_morph > 0 then 
        ps ("<script>\nwindow.onload = function () {window.location=\"" ^
            graph_cgi ^ "?" ^ text ^  
            ";cpts=" ^ (string_points checkpoints) ^ "\";}\n</script>")
     else ()
     (* Save sentence button *)
   ; if corpus_permission = Web_corpus.Annotator then
     (*i TODO: use [segment_iter] to compute the nb of sols instead of
        passing 0 to [nb_sols]. i*)
        save_button query 0 |> pl
     else () 
   ; html_break |> pl
     (* Quit button: continue reading (reader mode) 
                  or quit without saving (annotator mode) *)
   ; if sentence_no <> "" then
        quit_button corpus_permission
                    (decode_url corpus_dir) (decode_url sentence_no) |> pl
     else ()
   ; close_page_with_margin ()
   ; page_end lang True
   }
   with 
 [ Sys_error s         -> abort lang Control.sys_err_mess s (* file pb *)
 | Stream.Error s      -> abort lang Control.stream_err_mess s (* file pb *)
 | Encode.In_error s   -> abort lang "Wrong input " s
 | Exit (* Sanskrit *) -> abort lang "Wrong character in input" "" 
 | Overflow            -> abort lang "Maximum input size exceeded" ""
 | Invalid_argument s  -> abort lang Control.fatal_err_mess s (* sub array *)
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
end (* Interface *)
;
Interface.safe_engine () (* Should always produce a compliant HTML page *)
;

