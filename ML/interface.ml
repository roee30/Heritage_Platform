(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                        Gérard Huet & Pawan Goyal                       *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* Sanskrit Reader Summarizing interface. Yields sktgraph.cgi *)
(* We construct a CGI Interface displaying the segmentation graph in which the 
   user may indicate segments as mandatory checkpoints. At any point he may
   call the standard displaying of all, or of preferred solutions consistent 
   with the current checkpoints. An undo button allows backtracking. *)
module Interface =
  struct
    open Graph_segmenter
      
    (* [Segment cur_chunk set_cur_offset graph visual] *)
    open Phases
      
    (* [Phases] *)
    open Phases
      
    (* [phase is_cache generative] *)
    open Dispatcher
      
    (* [transducer_vect Dispatch transition trim_tags] *)
    open Html
      
    (* html constructors, escape *)
    open Web
      
    (* [ps pl abort reader_cgi scl_toggle] etc. *)
    open Cgi
      
    (* [url get decode_url] *)
    open Debug
      
    let ps s = output_string !Web.output_channel s
      
    let pl s = (s ^ "\n") |> ps
    module Prel =
      struct
        (* Interface's lexer prelude *)
        let prelude () =
          (pl http_header;
           page_begin graph_meta_title;
           pl (body_begin Chamois_back);
           pl interface_title;
           pl
             ((h3_begin C3) ^
                ("Click on " ^
                   ((html_green check_sign) ^
                      (" to select segment, click on " ^
                         ((html_red x_sign) ^
                            (" to rule out segment" ^ h3_end))))));
           pl
             ((h3_begin C3) ^
                (mouse_action_help ^
                   (" on segment to get its lemma" ^ h3_end)));
           open_page_with_margin 15)
          
      end
      
    (* Prel *)
    (* Service routines for morphological query, loading the morphology banks *)
    module Lemmas = Load_morphs.Morphs(Prel)(Phases)
      
    open Lemmas
      
    (* [tags_of morpho] *)
    open Load_transducers
      
    (* [Trans mk_transducers dummy_transducer_vect] *)
    (* Global parameters of the Lexer *)
    module Lexer_control =
      struct
        let star = ref true
          
        (* vaakya vs pada for Segment *)
        (* next is a reference holding the huge vector of all transducers *)
        let transducers_ref = ref (dummy_transducer_vect : transducer_vect)
          
      end
      
    (* [Lexer_control] *)
    module Transducers = Trans(Prel)
      
    module Machine = Dispatch(Transducers)(Lemmas)(Lexer_control)
      
    open Machine
      
    (* At this point we have a Finite Eilenberg machine ready to instantiate *)
    (* the Eilenberg component of the Segment module.                        *)
    (* Viccheda sandhi splitting *)
    module Viccheda = Segment(Phases)(Machine)(Lexer_control)
      
    open Viccheda
      
    (* [segment_iter visual_width] etc. *)
    (* At this point we have the sandhi inverser segmenting engine *)
    (* Display routines *)
    (* Separates tags of homophonous segments vertically *)
    let fold_vert f =
      let rec fold n =
        function
        | [] -> ()
        | [ x ] -> f n x
        | x :: l -> (f n x; ps html_break; fold (n + 1) l)
      in fold 1
      
    let print_morph pvs seg_num cached gen form n tag =
      Morpho_html.print_graph_link pvs cached form (seg_num, n) gen tag
      
    (* tags : Morphology.multitag is the multi-tag of the form of a given phase *)
    let print_tags pvs seg_num phase form tags =
      let gen = generative phase and cached = is_cache phase in
      let ok_tags =
        if pvs = []
        then tags
        else trim_tags (generative phase) form (Canon.decode pvs) tags
      and
        (* NB Existence of the segment warrants that [ok_tags] is not empty *)
        ptag = print_morph pvs seg_num cached gen form
      in fold_vert ptag ok_tags
      
      
    let pr_word w = (Canon.decode w) |> ps
      
    (* This is called "printing morphology interface style". *)
    let print_morpho phase word =
      match tags_of phase word with
      | Atomic tags -> print_tags [] 0 phase word tags
      | Preverbed ((_, ph), pvs, form, tags) -> print_tags pvs 0 ph form tags
      
    (* End of display routines *)
    (* Parsing mandatory checkpoints *)
    open Checkpoints
      
    (* [string_points] *)
    let rpc = Paths.remote_server_host
    and remote = ref false
      
    (* local invocation of cgi by default 
                          (switched on to [True] by "abs" cgi parameter) *)
    let invoke cgi = if !remote then rpc ^ cgi else cgi
      
    let mem_cpts ind phase_pada =
      let rec memrec =
        function
        | [] -> false
        | (k, pw, _) :: rest ->
            ((k = ind) && (pw = phase_pada)) || (memrec rest)
      in memrec
      
    let unanalysed (phase, _) = phase = Phases.unknown
      
    let already_checked = html_blue check_sign
      
    (*i TODO: call to undo this specific checkpoint i*)
    let call_back text cpts (k, seg) conflict =
      if mem_cpts k seg cpts
      then already_checked
      else
        if (not conflict) && (not (unanalysed seg))
        then already_checked
        else
          (let choices b = string_points ((k, seg, b) :: cpts)
           and (out_cgi, sign, color) =
             if unanalysed seg
             then (user_aid_cgi, spade_sign, Red_)
             else (graph_cgi, check_sign, Green_) in
           let call_back flag =
             out_cgi ^ ("?" ^ (text ^ (";cpts=" ^ (choices flag)))) in
           let cgi_select = call_back true
           and cgi_reject = call_back false
           in
             (anchor color (invoke cgi_select) sign) ^
               (if unanalysed seg
                then ""
                else anchor Red_ (invoke cgi_reject) x_sign))
      
    let call_reader text cpts mode = (* mode = "o", "p", "g" or "t" *)
      let cgi =
        reader_cgi ^
          ("?" ^
             (text ^ (";mode=" ^ (mode ^ (";cpts=" ^ (string_points cpts))))))
      in anchor Green_ (invoke cgi) check_sign
      
    let call_parser text cpts =
      let cgi =
        parser_cgi ^
          ("?" ^
             (text ^
                (";mode=p" ^ (";cpts=" ^ ((string_points cpts) ^ ";n=1")))))
      in anchor Green_ (invoke cgi) check_sign
      
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
    let sort_check cpts =
      let compare_index (a, _, _) (b, _, _) = compare a b
      in List.sort compare_index cpts
      
    let seg_length =
      function
      | (-2) :: rest -> Word.length rest
      | (* lopa does not count *) w -> Word.length w
      
    let rec merge_rec lpw =
      function
      | [] -> lpw
      | (p, lw) :: rest ->
          let rec fill p lpw =
            (function
             | [] -> lpw
             | wh :: rest1 -> fill p ((p, wh) :: lpw) rest1)
          in merge_rec (fill p lpw lw) rest
      
    let build_visual k segments =
      if segments = []
      then ()
      else
        (let phw = merge_rec [] segments in
         let comp_length (_, (a, _)) (_, (b, _)) =
           compare (seg_length a) (seg_length b) in
         let sorted_seg = List.rev (List.sort comp_length phw) in
         let rec ass_rec seg =
           let start_ind =
             let rec find_ind_rec n =
               if k < visual_width.(n) then find_ind_rec (n + 1) else n
             in find_ind_rec 0
           in
             match seg with
             | [] -> ()
             | (phase, (w1, tr)) :: rest ->
                 if preverb_phase phase
                 then raise (Control.Anomaly "Phantom preverb")
                 else (* preverbs have been filtered out by Dispatch *)
                   (visual.(start_ind) <-
                      visual.(start_ind) @ [ (w1, tr, phase, k) ];
                    visual_width.(start_ind) <- (seg_length w1) + k;
                    ass_rec rest)
         in ass_rec sorted_seg)
      
    (* We check whether the current segment [(w,tr,phase,k)] is conflicting with 
   others at previous offset [n]; if not it is mandatory and marked blue.      *)
    (* Returns True for blue mandatory segments, False for green/red optional ones *)
    (* Warning: very hairy code, do not change without thorough understanding .    *)
    let is_conflicting (((w, tr, ph, k) as segment)) =
      let l = seg_length w in
      let rec is_conflicting_rec n = (* n is position in input string *)
        match visual.(n) with
        | [] -> false
        | (* will exit here when n is length of input *) segs ->
            let rec (* we search for conflicting segments *) does_conflict 
              =
              (function
               | [] -> is_conflicting_rec (n + 1)
               | (* go to next input position *)
                   (((w', tr', ph', k') as segment')) :: rest ->
                   if segment' = segment
                   then (* skip itself *) does_conflict rest
                   else
                     (let l' = seg_length w'
                      in
                        if
                          ((k' <= k) && (((k' + l') - 1) > k)) ||
                            (* w inside w' *)
                            (((k' <= k) &&
                                ((((k' + l') - 1) >= k) && (l = 1)))
                               || (* w is a or aa *)
                               (* This condition is necessary for the overlapping case *)
                               ((k <= k') && (((k + l) - 1) > k')))
                        then
                          if ((k + l) - 1) = k'
                          then
                            (let r' = Word.mirror w' in
                             let rec
                               (* This is to check for the overlapping case, occurs when [k=k', l=1]. 
         We need to check the sandhi conditions to decide whether this is a case 
         of overlap or conflict. *)
                               match_tr =
                               function
                               | [] -> true
                               | v :: rst ->
                                   (match v with
                                    | [] -> match_tr rst
                                    | _ ->
                                        if Word.prefix v r'
                                        then does_conflict rest
                                        else match_tr rst)
                             in match_tr tr)
                          else
                            if
                              (k' <= k) && ((((k' + l') - 1) = k) && (l = 1))
                            then
                              (let (* For the case with [l=1], this is to check whether w is the only 
         possible v for w', in which case it is an overlap returning a blue sign.
         If w' has any other possible v's, there is a conflict. *)
                                 (* This may only occur if w=[1] (a) and w' ends in a or aa *)
                                 (* e.g. In "naabhaava.h caakiirti.h",  "a" should be marked blue, and in
         "mahaajana.h" after checking "mahaa", "a" should not be marked blue *)
                                 match_tr' =
                                 function
                                 | [ v ] ->
                                     (not (v = w)) || (does_conflict rest)
                                 | _ -> true
                               in match_tr' tr')
                            else true
                        else does_conflict rest))
            in does_conflict segs
      in is_conflicting_rec 0
      
    (* Remaining bug: "mahaabaho" when deleting "a", "ap" goes blue despite "baho" *)
    let rec find_conflict_seg acc l =
      function
      | [] -> List.rev acc
      | (w1, tr, phase, k) :: rest ->
          let conflict = is_conflicting (w1, tr, phase, k) in
          let seg_here = (w1, phase, k, conflict)
          in find_conflict_seg (seg_here :: acc) l rest
      
    let rec find_conflict (l : int) =
      match visual.(l) with
      | [] -> ()
      | segs ->
          (visual_conf.(l) <- find_conflict_seg [] l segs;
           find_conflict (succ l))
      
    let make_visual n =
      let rec vrec (k : int) =
        (build_visual k graph.(k); if k = (n - 1) then () else vrec (succ k))
      in vrec 0
      
    let rec print_extra =
      function | 0 -> () | l -> ((td_wrap "") |> ps; print_extra (l - 1))
    and fixed_space = td_wrap "&nbsp;"
      
    let rec print_first_server chunk =
      match Word.length chunk with
      | 0 -> ps fixed_space
      | l ->
          (match chunk with
           | [] -> fixed_space |> ps
           | st :: rest ->
               let to_print = Canon.uniromcode [ st ]
               in ((td_wrap to_print) |> ps; print_first_server rest))
      
    let call_back_pseudo text cpts ph newpt =
      if List.mem newpt cpts
      then already_checked
      else
        (let list_points = newpt :: cpts in
         let out_cgi = user_aid_cgi in
         let cgi =
           out_cgi ^
             ("?" ^ (text ^ (";cpts=" ^ (string_points list_points))))
         in anchor_pseudo (invoke cgi) ph)
      
    let un_analyzable (chunk : Word.word) =
      (Phases.Unknown, (Word.mirror chunk))
      
    let rec print_first text cpts chunk_orig chunk chunk_ind =
      match Word.length chunk with
      | 0 -> ps fixed_space
      | l ->
          (match chunk with
           | [] -> fixed_space |> ps
           | st :: rest ->
               let to_print = Canon.uniromcode [ st ]
               in
                 ((let unknown_chunk =
                     (chunk_ind, (un_analyzable chunk_orig), true)
                   in
                     (td_wrap
                        (call_back_pseudo text cpts to_print unknown_chunk))
                       |> ps);
                  print_first text cpts chunk_orig rest chunk_ind))
      
    (* Making use of the index for printing the chunk callback *)
    let rec print_all text cpts chunks index =
      match chunks with
      | [] -> ()
      | chunk :: rest ->
          (print_first text cpts chunk chunk index;
           print_all text cpts rest (succ (Word.length chunk)))
      
    let print_word last_ind text cpts
                   ((rword : Word.word), phase, k, conflict) =
      let (word : Word.word) = Word.mirror rword
      in
        ((let extra_space = k - last_ind
          in if extra_space > 0 then print_extra extra_space else ());
         (*i ZZ following not implementable with a fixed css -- not HTML5 compliant i*)
         (td_begin_att
            [ ("colspan", (string_of_int (seg_length word)));
              ("align", "left") ])
           |> ps;
         (let back = background (color_of_phase phase)
          in (table_begin back) |> pl);
         tr_begin |> ps;
         "<td class='tooltip'>" |> ps;
         Morpho_html.print_final rword;
         (* visarga correction *)
         ps "<span class='tooltiptext'>";
         _debug "print_word {{{";
         print_morpho phase word;
         _debug "print_word }}}";
         (* ; let close_box =  *)
         (*       "<a>" ^ x_sign ^ "</a>" in  *)
         (*   close_box |> ps *)
         ps "</span>";
         td_end |> ps;
         tr_end |> ps;
         table_end |> ps;
         (*; call_back text cpts (k,(phase,rword)) conflict |> ps*)
         td_end |> ps)
      
    let max_col = ref 0
      
    let print_row text cpts =
      let rec print_this text cpts last_ind =
        function
        | [] ->
            let adjust = !max_col - last_ind
            in if adjust > 0 then print_extra adjust else ()
        | (word, phase, k, conflict) :: rest ->
            (_debug "print_word2 {{{";
             print_word last_ind text cpts (word, phase, k, conflict);
             _debug "print_word2 }}}";
             print_this text cpts (k + (seg_length word)) rest)
      in print_this text cpts 0
      
    let print_interf text cpts () =
      let rec vgrec (k : int) =
        match visual_width.(k) with
        | 0 -> ()
        | _ ->
            (tr_begin |> ps;
             (*visual_conf: (Word.word * Phases.Phases.phase * int * bool) list*)
             _debug (string_of_int (List.length visual_conf.(k)));
             print_row text cpts visual_conf.(k);
             tr_end |> ps;
             vgrec (succ k))
      in vgrec 0
      
    let update_col_length chunk =
      max_col := succ (!max_col + (Word.length chunk))
      
    let call_undo text cpts =
      let string_pts =
        match cpts with
        | [] -> ""
        | (* Could raise warning "undo stack empty" *) _ :: rest ->
            string_points rest in
      let cgi = graph_cgi ^ ("?" ^ (text ^ (";cpts=" ^ string_pts)))
      in anchor Green_ (invoke cgi) check_sign
      
    (* The main procedure for computing the graph segmentation structure *)
    let check_sentence translit uns text checkpoints input undo_enabled =
      let encode = Encode.switch_code translit in
      let chunker =
        if uns
        then (* sandhi undone *) Sanskrit.read_raw_sanskrit
        else (* chunking *) Sanskrit.read_sanskrit in
      let raw_chunks = Sanskrit.read_raw_sanskrit encode input in (* NEW *)
      let chunks = chunker encode input in
      let deva_chunks = List.map Canon.unidevcode raw_chunks in (* NEW *)
      let deva_input = String.concat " " deva_chunks
      and cpts = sort_check checkpoints in
      let _ = chkpts.all_checks <- cpts
      and (full, count) = segment_iter chunks
      in
        ((* full iff all chunks segment *)
         make_visual cur_chunk.offset;
         find_conflict 0;
         html_break |> pl;
         (*; html_latin16 "Sentence: " |> pl*)
         (*; deva16_blue deva_input |> ps (* devanagari *)*)
         html_break |> ps;
         (div_begin Latin16) |> ps;
         (table_begin Spacing20) |> pl;
         tr_begin |> pl;
         (* tr begin *)
         (*; if undo_enabled then *)
         (*     td_wrap (call_undo text checkpoints ^ "Undo") |> ps*)
         (*  else ()*)
         (let call_scl_parser () = (* invocation of scl parser *)
            if scl_toggle
            then
              (td_wrap ((call_reader text cpts "o") ^ "UoH Analysis Mode"))
                |> ps
            else ()
          in
            (* [scl_parser] is not visible unless toggle is set *)
            (*if count > Web.max_count then *)
            (*   (* too many solutions would choke the parsers *) *)
            (*   td_wrap ("(" ^ string_of_int count ^ " Solutions)") |> ps*)
            (*else if count=1 (* Unique remaining solution *) then do*)
            (*        { td_wrap (call_parser text cpts ^ "Unique Solution") |> ps*)
            (*        ; call_scl_parser ()*)
            (*        }*)
            (*     else do*)
            (*   { td_wrap (call_reader text cpts "p" ^ "Filtered Solutions") |> ps*)
            (*   ; let info = string_of_int count ^ if full then "" else " Partial" in *)
            (*     td_wrap (call_reader text cpts "t" ^ "All " ^ info ^ " Solutions") |> ps*)
            (*   ; call_scl_parser ()*)
            (*   } *)
            tr_end |> pl);
         (* tr end *)
         table_end |> pl;
         div_end |> ps;
         (* Latin16 *)
         html_break |> pl;
         (div_begin Latin12) |> ps;
         (table_begin Tcenter) |> pl;
         tr_begin |> ps;
         List.iter update_col_length chunks;
         if Paths.platform = "Station"
         then print_all text checkpoints chunks 0
         else List.iter print_first_server chunks;
         tr_end |> pl;
         print_interf text checkpoints ();
         table_end |> pl;
         div_end |> ps;
         (* Latin12 *)
         html_break |> pl)
      
    let arguments trans lex font cache st us input topic abs
                  corpus_permission corpus_dir sentence_no =
      "t=" ^
        (trans ^
           (";lex=" ^
              (lex ^
                 (";font=" ^
                    (font ^
                       (";cache=" ^
                          (cache ^
                             (";st=" ^
                                (st ^
                                   (";us=" ^
                                      (us ^
                                         (";text=" ^
                                            (input ^
                                               (";topic=" ^
                                                  (topic ^
                                                     (";abs=" ^
                                                        (abs ^
                                                           (";" ^
                                                              (Params.
                                                                 corpus_permission
                                                                 ^
                                                                 ("=" ^
                                                                    (
                                                                    corpus_permission
                                                                    ^
                                                                    (";" ^
                                                                    (Params.
                                                                    corpus_dir
                                                                    ^
                                                                    ("=" ^
                                                                    (corpus_dir
                                                                    ^
                                                                    (";" ^
                                                                    (Params.
                                                                    sentence_no
                                                                    ^
                                                                    ("=" ^
                                                                    sentence_no))))))))))))))))))))))))))))
      
    (* Cache management *)
    (* [ (Morphology.inflected_map * Morphology.inflected_map) -> unit] *)
    let make_cache_transducers (cache, cachei) =
      let deco_cache = Mini.minimize (Deco.forget_deco cache)
      and deco_cachei = Mini.minimize (Deco.forget_deco cachei) in
      let auto_cache = Automaton.compile Deco.empty deco_cache
      and auto_cachei = Automaton.compile Deco.empty deco_cachei
      in
        (Gen.dump cache Data.public_cache_file;
         (* for [Load_morphs] *)
         Gen.dump cachei Data.public_cachei_file;
         (* id *)
         Gen.dump auto_cache Data.public_trans_cache_file;
         (* for [Load_transducers] *)
         Gen.dump auto_cachei Data.public_trans_cachei_file)
      
    (* id *)
    (* We fill gendered entries incrementally in [public_cache_txt_file] *)
    let append_cache entry gender =
      let cho =
        open_out_gen [ Open_wronly; Open_append; Open_text ] 0o777 Data.
          public_cache_txt_file
      in
        (output_string cho ("[{" ^ (entry ^ ("}] ({" ^ (gender ^ "})\n"))));
         close_out cho)
      
    (* Corpus management : saving an analysed sentence in corpus *)
    let save_button query nb_sols =
      center_begin ^
        ((cgi_begin save_corpus_cgi "") ^
           ((hidden_input Save_corpus_params.state (escape query)) ^
              ((hidden_input Save_corpus_params.nb_sols
                  ((nb_sols |> string_of_int) |> escape))
                 ^ ((submit_input "Save") ^ (cgi_end ^ center_end)))))
      
    let quit_button corpmode corpdir sentno =
      let submit_button_label = let open Web_corpus
        in
          match corpmode with
          | Annotator -> "Abort"
          | Reader | Manager -> "Continue reading"
      and permission = Web_corpus.string_of_permission corpmode
      in
        center_begin ^
          ((cgi_begin (url corpus_manager_cgi ~fragment: sentno) "") ^
             ((hidden_input Params.corpus_dir corpdir) ^
                ((hidden_input Params.corpus_permission permission) ^
                   ((submit_input submit_button_label) ^
                      (cgi_end ^ center_end)))))
      
    (* Main body of sktgraph cgi *)
    let graph_engine () =
      ((*Prel.prelude () ;*)
       Printexc.record_backtrace true;
       let query = Sys.getenv "QUERY_STRING" in
       let env = create_env query in
       (* Multiple environment variables according to modes of use are: 
       text topic st us t lex font cache abs cpts (standard mode) 
       corpdir sentno corpmode (defined in Params) 
       guess gender revised [rev_off] [rev_ind] (User-aid) *)
       let _url_encoded_input : string = get "text" env "" in
       (*let () = "<h1>" ^ _url_encoded_input ^ "</h1>" |> ps in*)
       let url_encoded_input : string =
         Encode.devanagari_to_velthuis _url_encoded_input in
       (*let () = "<h1>" ^ url_encoded_input ^ "</h1>" |> ps in*)
       let url_encoded_topic = get "topic" env ""
       and (* topic carry-over *) st = get "st" env "t"
       and (* sentence parse default *) us = get "us" env "f"
       and (* sandhied text default *) translit =
         get "t" env Paths.default_transliteration
       and (* translit input *) lex = get "lex" env Paths.default_lexicon
       and (* lexicon choice *) font =
         get "font" env Paths.default_display_font in
       let ft = font_of_string font
       and (* Deva vs Roma print *) cache = get "cache" env "f" in
       (* no cache default *)
       (* ft and cache are persistent in the session *)
       let () = toggle_lexicon lex
       and (* sticky lexicon switch *) () = toggle_sanskrit_font ft
       and () = cache_active := cache and abs = get "abs" env "f" in
       (* default local paths *)
       let lang = language_of_string lex
       and (* lexicon indexing choice *) input : string =
         decode_url url_encoded_input
       and (* unnormalized string *) uns = us = "t"
       and (* unsandhied vs sandhied corpus *) () =
         if st = "f" then Lexer_control.star := false else ()
       and (* word vs sentence stemmer *) () =
         Lexer_control.transducers_ref := Transducers.mk_transducers ()
       and url_enc_corpus_permission = (* Corpus mode *)
         get Params.corpus_permission env "true" in
       let corpus_permission =
         (url_enc_corpus_permission |> decode_url) |> Web_corpus.
           permission_of_string in
       let corpus_dir = get Params.corpus_dir env ""
       and sentence_no = get Params.sentence_no env "" in
       let undo_enabled =
         (sentence_no = "") || (* no undo in Reader corpus mode *)
           (corpus_permission <> Web_corpus.Reader) in
       let text : string =
         arguments translit lex font cache st us url_encoded_input
           url_encoded_topic abs url_enc_corpus_permission corpus_dir
           sentence_no
       and checkpoints =
         try
           let url_encoded_cpts = List.assoc "cpts" env
           in (* do not use get *) parse_cpts (decode_url url_encoded_cpts)
         with | Not_found -> []
       and (* Now we check if cache acquisition is required *) guess_morph 
         = decode_url (get "guess" env "")
       and (* User-aid guessing *) pseudo_gender =
         decode_url (get "gender" env "") in
       let _ =
         if ((String.length guess_morph) > 0) && (Paths.platform = "Station")
         then (* User-aid cache acquisition *)
           (let (entry, gender) =
              match pseudo_gender with
              | "" -> parse_guess guess_morph
              | g -> (guess_morph, g)
            in
              (append_cache entry gender;
               let cache_txt_file = Data.public_cache_txt_file in
               let caches = Nouns.extract_current_caches cache_txt_file
               in make_cache_transducers caches))
         else () in
       let revised = decode_url (get "revised" env "")
       and (* User-aid revision *) rev_off =
         int_of_string (get "rev_off" env "-1")
       and rev_ind = int_of_string (get "rev_ind" env "-1")
       in
         try
           ((match (revised, rev_off, rev_ind) with
             | ("", (-1), (-1)) ->
                 (* Standard input processing *** Main call *** *)
                 check_sentence translit uns text checkpoints input
                   undo_enabled
             | (new_word, word_off, chunk_ind) ->
                 (* User-aid revision mode *)
                 let chunks =
                   Sanskrit.read_sanskrit (Encode.switch_code translit) input in
                 let rec decoded init ind =
                   (function
                    | [] -> String.sub init 0 ((String.length init) - 1)
                    | a :: rest ->
                        let ind' = ind + 1
                        and init' =
                          if ind = chunk_ind
                          then init ^ (new_word ^ "+")
                          else
                            init ^ ((Canon.switch_decode translit a) ^ "+")
                        in decoded init' ind' rest) in
                 let updated_input = decoded "" 1 chunks in
                 let rec find_word_len cur_ind =
                   (function
                    | [] -> 0
                    | a :: rest ->
                        if cur_ind = chunk_ind
                        then Word.length a
                        else find_word_len (cur_ind + 1) rest) in
                 let word_len = find_word_len 1 chunks
                 and new_chunk_len =
                   Word.length (Encode.switch_code translit revised) in
                 let diff = new_chunk_len - word_len in
                 let revised_check =
                   let revise (k, sec, sel) =
                     ((if k < word_off then k else k + diff), sec, sel)
                   in List.map revise checkpoints
                 and new_text =
                   arguments translit lex font cache st us updated_input
                     url_encoded_topic abs url_enc_corpus_permission
                     corpus_dir sentence_no
                 and new_input = decode_url updated_input
                 in
                   (_debug "check_sentence {{{";
                    check_sentence translit uns new_text revised_check
                      new_input undo_enabled;
                    _debug "check_sentence }}}"));
            (* Rest of the code concerns Corpus mode *)
            (* automatically refreshing the page only if guess parameter *)
            if (String.length guess_morph) > 0
            then
              ps
                ("<script>\nwindow.onload = function () {window.location=\""
                   ^
                   (graph_cgi ^
                      ("?" ^
                         (text ^
                            (";cpts=" ^
                               ((string_points checkpoints) ^
                                  "\";}\n</script>"))))))
            else ();
            (* Save sentence button *)
            if corpus_permission = Web_corpus.Annotator
            then
              (*i TODO: use [segment_iter] to compute the nb of sols instead of
        passing 0 to [nb_sols]. i*)
              (save_button query 0) |> pl
            else ();
            html_break |> pl;
            (* Quit button: continue reading (reader mode) 
                  or quit without saving (annotator mode) *)
            if sentence_no <> ""
            then
              (quit_button corpus_permission (decode_url corpus_dir)
                 (decode_url sentence_no))
                |> pl
            else ();
            close_page_with_margin ())
         with
         | (*; page_end lang True*) Sys_error s ->
             abort lang Control.sys_err_mess s
         | (* file pb *) Stream.Error s ->
             abort lang Control.stream_err_mess s
         | (* file pb *) Encode.In_error s -> abort lang "Wrong input " s
         | Exit -> (* Sanskrit *) abort lang "Wrong character in input" ""
         | Overflow -> abort lang "Maximum input size exceeded" ""
         | Invalid_argument s -> abort lang Control.fatal_err_mess s
         (*| (* sub array *) Failure s -> abort lang Control.fatal_err_mess s*)
         | (* anomaly *) End_of_file ->
             abort lang Control.fatal_err_mess "EOF"
         | (* EOF *) Not_found ->
             let s = "You must choose a parsing option"
             in abort lang "Unset button in form - " s
         | Control.Fatal s -> abort lang Control.fatal_err_mess s
         | (* anomaly *) Control.Anomaly s ->
             abort lang Control.anomaly_err_mess s
         | e -> raise e)
      
    (*| _                   -> abort lang Control.fatal_err_mess "Unexpected anomaly" *)
    let safe_engine () =
      (* Problem: in case of error, we lose the current language of the session *)
      let abor = abort default_language
      in
        try graph_engine ()
        with
        | (* Failure s -> abor Control.fatal_err_mess s (* [parse_cpts phase_string] ? *) |  *)
            e -> raise e
      
  end
  
(*| _ -> abor Control.fatal_err_mess "Unexpected anomaly - broken session" *)
(* Interface *)
let _ = Interface.safe_engine ()
  

