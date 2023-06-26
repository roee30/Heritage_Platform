(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*              Gérard Huet & Pawan Goyal & Sriram Krishnan               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Sanskrit Reader Summarizing interface. Yields sktgraph2.cgi *)

(* We construct a CGI Interface displaying the segmentation graph in which the 
   user may indicate segments as mandatory checkpoints. At any point he may
   call the standard displaying of all, or of preferred solutions consistent 
   with the current checkpoints. An undo button allows backtracking. *)

module Interface2 = struct

open Graph_segmenter2; (* [Segment cur_chunk set_cur_offset graph visual] *)
open Phases; (* [Phases] *) 
open Phases; (* [phase is_cache generative] *) 
open Dispatcher; (* [transducer_vect Dispatch transition trim_tags] *) 
open Html; (* html constructors, escape *)
open Web; (* [ps pl abort reader_cgi scl_toggle] etc. *) 
open Cgi; (* [url get decode_url] *)

module Prel = struct (* Interface2's lexer prelude *)

 value prelude () = do
  { pl http_header
  ; page_begin graph_meta_title 
  ; pl (body_begin Chamois_back)
  ; pl interface_title
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
open Load_transducers (* [Trans mk_transducers dummy_transducer_vect] *)
;
(* Global parameters of the Lexer *)
module Lexer_control = struct
 value star = ref True; (* vaakya vs pada for Segment *)
 (* next is a reference holding the huge vector of all transducers *)
 value transducers_ref = ref (dummy_transducer_vect : transducer_vect); 
 end (* [Lexer_control] *)
;
module Transducers = Trans Prel 
;
module Machine = Dispatch Transducers Lemmas Lexer_control
;
open Machine 
;
(* At this point we have a Finite Eilenberg machine ready to instantiate *)
(* the Eilenberg component of the Segment module.                        *)
open Morpho_analysis;
module Morpho_anal = Morpho_analysis Phases Lemmas Machine;
open Morpho_anal;
open Frequency;
module Freq = Frequency Phases Machine Morpho_anal;
open Freq;
(* Viccheda sandhi splitting *)
module Viccheda = Segment2 Phases Machine Lexer_control Freq
;
open Viccheda (* [segment_iter visual_width] etc. *)
;

(* Graph interface2 *)
(* Mode parameter of the interface. Controled by service Interface2 
     for respectively Summary or Best solutions *)
(* Note that Interface2 is not a Reader/Parser mode. *)
type best_mode = [ First_Summary | First_List | Best_Summary | Best_List ]
;
value mode_id_of_mode mode = 
  match mode with
  [ Best_Summary -> "b"
  | Best_List -> "l"
  | First_Summary -> "f"
  | First_List -> "s"
  | _ -> raise (Failure ("Unknown mode type"))
  ] 
;
value mode_of_mode_id mode_id = 
  match mode_id with
  [ "b" -> Best_Summary
  | "l" -> Best_List
  | "f" -> First_Summary
  | "s" -> First_List
  | x -> raise (Failure ("Unknown mode " ^ x))
  ] 
;
(* At this point we have the sandhi inverser segmenting engine *)

(* Display routines *)

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
        | Preverbed (_,ph) pvs form tags -> print_tags pvs 0 ph form tags
        ]
;
(* End of display routines *) 

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
value call_back text mode cpts rcpts (k,seg) conflict = 
  if mem_cpts k seg cpts then already_checked 
  else if not conflict && not (unanalysed seg) then already_checked
  else let choices b = string_points [ (k,seg,b) :: cpts ] 
       and (out_cgi,sign,color) = 
           if unanalysed seg then (user_aid_cgi,spade_sign,Red_) 
                             else (graph_cgi2,   check_sign,Green_) in
       let call_back flag = out_cgi ^ "?" ^ text ^ ";mode=" ^ mode ^ 
                            ";cpts=" ^ choices flag ^ 
                            ";rcpts=" ^ (string_points rcpts) in
       let cgi_select = call_back True
       and cgi_reject = call_back False in
       anchor color (invoke cgi_select) sign ^ 
          if unanalysed seg then "" else anchor Red_ (invoke cgi_reject) x_sign
;
value call_reader text cpts rcpts mode cur_mode fmode = 
  (* mode = "o", "p", "g", "t" - for overall 
     [cur_mode] = "b", "f", "l", "s" - for best *) 
  let cgi = reader_cgi ^ "?" ^ text ^ ";mode=" ^ mode 
            ^ ";best_mode=" ^ cur_mode 
            ^ ";fmode=" ^ fmode 
            ^ ";cpts=" ^ (string_points cpts) 
            ^ ";rcpts=" ^ (string_points rcpts) in 
  anchor Green_ (invoke cgi) check_sign
;
value call_parser text cpts =
  let cgi = parser_cgi ^ "?" ^ text ^ ";mode=p" ^ 
            ";cpts=" ^ string_points cpts ^ ";n=1" in
  anchor Green_ (invoke cgi) check_sign
;
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
      | [ (phase,(w1,tr)) :: rest ] -> 
            if preverb_phase phase then raise (Control.Anomaly "Phantom preverb")
            else (* preverbs have been filtered out by Dispatch *) do
              { visual.(start_ind) := visual.(start_ind) @ [ (w1,tr,phase,k) ]
              ; visual_width.(start_ind) := (seg_length w1) + k
              ; ass_rec rest
              }
      ]
;
(* We check whether the current segment [(w,tr,phase,k)] is conflicting with 
   others at previous offset [n]; if not it is mandatory and marked blue.      *)
(* Returns True for blue mandatory segments, False for green/red optional ones *)
(* Warning: very hairy code, do not change without thorough understanding .    *)
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
  | l -> do { td_wrap "" |> ps; print_extra (l-1) }
  ]
and fixed_space = td_wrap "&nbsp;"
;
value rec print_first_server chunk = 
  match Word.length chunk with
  [ 0 -> ps fixed_space
  | l -> match chunk with 
         [ [] -> fixed_space |> ps
         | [ st :: rest ] -> let to_print = Canon.uniromcode [ st ] in do
             { td_wrap to_print |> ps
             ; print_first_server rest
             }
         ]
  ]
;
value call_back_pseudo text mode cpts rcpts ph newpt =
  if List.mem newpt cpts then already_checked 
  else let list_points = [ newpt :: cpts ] in
       let out_cgi = user_aid_cgi in
       let cgi = out_cgi ^ "?" ^ text ^ ";mode=" ^ mode ^ 
                 ";cpts=" ^ (string_points list_points) ^ 
                 ";rcpts=" ^ (string_points rcpts) in
       anchor_pseudo (invoke cgi) ph
;
value un_analyzable (chunk : Word.word) = (Phases.Unknown,Word.mirror chunk)
;
value rec print_first text mode cpts rcpts chunk_orig chunk chunk_ind = 
  match Word.length chunk with
  [ 0 -> ps fixed_space
  | l -> match chunk with 
         [ [] -> fixed_space |> ps
         | [ st :: rest ] -> let to_print = Canon.uniromcode [ st ] in do
             { let unknown_chunk = (chunk_ind,un_analyzable chunk_orig,True) in
               td_wrap (call_back_pseudo text mode cpts rcpts 
                                         to_print unknown_chunk) |> ps
             ; print_first text mode cpts rcpts chunk_orig rest chunk_ind
             }
         ]
  ]
 ;
(* Making use of the index for printing the chunk callback *)
value rec print_all text mode cpts rcpts chunks index = match chunks with
  [ [] -> ()
  | [ chunk :: rest ] -> do
      { print_first text mode cpts rcpts chunk chunk index
      ; print_all text mode cpts rcpts rest (succ (Word.length chunk))
      }
  ]
;
value print_word last_ind text mode cpts rcpts (rword,phase,k,conflict) = 
  let word = Word.mirror rword in do
  { let extra_space = k-last_ind in 
    if extra_space > 0 then print_extra extra_space else ()
(*i ZZ following not implementable with a fixed css -- not HTML5 compliant i*)
  ; td_begin_att [ ("colspan",string_of_int (seg_length word))
                 ; ("align","left") 
                 ] |> ps
  ; let back = background (color_of_phase phase) in 
    table_begin back |> pl
  ; tr_begin |> ps
  ; "<td " ^ display_morph_action ^ "=\"showBox('" |> ps
  ; print_morpho phase word 
  ; let close_box = 
        "<a href=&quot;javascript:hideBox()&quot;> " ^ x_sign ^ "</a>', '" in 
    close_box ^ rgb (color_of_phase phase) ^ "', this, event)\">" |> ps
  ; Morpho_html.print_final rword (* visarga correction *)
  ; td_end |> ps
  ; tr_end |> ps
  ; table_end |> ps 
  ; call_back text mode cpts rcpts (k,(phase,rword)) conflict |> ps
  ; td_end |> ps
  }
;
(* Prints the word-form with phase details *)
value print_best_word (phase,rword) = 
  let word = Word.mirror rword in do
  { td_begin_att [ ("align","left") ] |> ps
  ; let back = background (color_of_phase phase) in 
    table_begin back |> pl
  ; "<td " ^ display_morph_action ^ "=\"showBox('" |> ps
  ; print_morpho phase word 
  ; let close_box = 
        "<a href=&quot;javascript:hideBox()&quot;> " ^ x_sign ^ "</a>', '" in 
    close_box ^ rgb (color_of_phase phase) ^ "', this, event)\">" |> ps
  ; Morpho_html.print_final rword (* visarga correction *)
  ; td_end |> ps
  ; table_end |> ps
  ; td_end |> ps
  }
;
value max_col = ref 0
;
value print_row text mode cpts rcpts =  print_this text cpts 0 
  where rec print_this text cpts last_ind = fun 
  [ [] -> let adjust = max_col.val - last_ind in
          if adjust > 0 then print_extra adjust else ()
  | [ (word,phase,k,conflict) :: rest ] -> do 
      { print_word last_ind text mode cpts rcpts (word,phase,k,conflict)
      ; print_this text cpts (k + seg_length word) rest
      }
  ]
;
value print_interf text mode cpts rcpts () = vgrec 0 
  where rec vgrec k = 
  match visual_width.(k) with
  [ 0 -> ()
  | _ -> do
    { tr_begin |> ps
    ; print_row text mode cpts rcpts visual_conf.(k) 
    ; tr_end |> ps
    ; vgrec (succ k)
    }
  ]
;
value update_col_length chunk = 
  max_col.val := succ (max_col.val + Word.length chunk)
;
value call_undo text mode cpts rcpts  = 
  let string_pts = match cpts with 
      [ [] -> "" (* Could raise warning "undo stack empty" *)
      | [ _ :: rest ] -> string_points rest
      ] in
  let cgi = graph_cgi2 ^ "?" ^ text ^ ";mode=" ^ mode ^ ";cpts=" ^ string_pts ^ 
            ";rcpts=" ^ (string_points rcpts) in
  anchor Green_ (invoke cgi) check_sign
;
(* Summary of All Solutions *)
value call_full_graph text = 
  let cgi = graph_cgi ^ "?" ^ text ^ ";mode=g" in
  let invocation = if remote.val then rpc ^ cgi else cgi in
  anchor Green_ invocation check_sign
;
(* Summary of best n solutions *)
value call_best_graph text mode_id cpts rcpts = 
  let cgi = graph_cgi2 ^ "?" ^ text ^ ";mode=" ^ mode_id ^ ";cpts=" ^ 
            (string_points cpts) ^ ";rcpts=" ^ (string_points rcpts) in
  let invocation = if remote.val then rpc ^ cgi else cgi in
  anchor Green_ invocation check_sign
;
(* List of best n solutions *)
value call_best_list text mode_id cpts rcpts = 
  let cgi = graph_cgi2 ^ "?" ^ text ^ ";mode=" ^ mode_id ^ ";cpts=" ^ 
            (string_points cpts) ^ ";rcpts=" ^ (string_points rcpts) in
  let invocation = if remote.val then rpc ^ cgi else cgi in
  anchor Green_ invocation check_sign
;
(* Accessing SCL's default font *)
value scl_font = match sanskrit_font.val with 
  [ Deva -> "Devanagari"
  | Roma -> "IAST"
  ]
;
(* To get the word from triple *)
value get_sandhi_word font (phase,rword) = 
  let visargified_word = (Morpho_html.visargify rword) in
  let decode_word = 
    if font = "roma" then Canon.uniromcode visargified_word
    else if font  = "wx" then Canon.decode_WX visargified_word
    else if font  = "deva" then Canon.unidevcode visargified_word
    (* Temporarily returning in WX notation for all other cases *)
    else Canon.decode_WX visargified_word in 
  if (ii_component phase) then (decode_word ^ "-")
  else (decode_word ^ " ")
;
(* Getting the sentence from the list of segments *)
value get_sentence font output = 
  loop "" output
  where rec loop acc = fun
  [ [] -> acc
  | [(phase, rword) :: tl] -> 
      loop (acc ^ (get_sandhi_word font (phase, rword))) tl
  ]
;
value call_scl_segments text id mode_id fmode_id output = 
  let get_chkpt (index, (phase, word, transition)) = 
    (index, (phase, word), True) in 
  let checkpoints = List.map get_chkpt output in 
  call_reader text checkpoints [] "o" mode_id fmode_id ^ (string_of_int id)
;
value print_scl_link scl_font id segmentation = do 
  { td_begin_att [ ("align","left") ] |> ps
  ; table_begin Latin12 |> pl
  ; Scl_parser.print_scl_segmentation scl_font id segmentation
  ; table_end |> ps
  ; td_end |> ps
  }
;
(* Prints segmentation word-forms (along with phase details) in list view 
   with a link to SCL *)
value print_scl_segments text id mode_id fmode_id segmentation output = do
  { table_begin Latin12 |> pl
  ; tr_begin |> ps
  ; List.iter print_best_word segmentation 
  ; if scl_toggle then
      print_scl_link scl_font id segmentation 
    (* Either run the SCL's script directly as above or 
       call UoH Analysis mode in Reader page as below.
       The UoH Analysis mode in the Reader page sometimes
       produces multiple solutions even if there is only one
       possible solution. So, temporarily the following is 
       commented. *)
    (* [td_wrap (call_scl_segments text id mode_id fmode_id output) |> ps] *)
    else ()
  ; tr_end |> ps
  ; table_end |> ps 
  }
;
(* Call the SCL's parser directly with a link. *)
value call_scl_text text font = 
  let cgi = "/cgi-bin/scl/MT/anusaaraka.cgi?encoding=WX&text=" ^ text 
            ^ "&splitter=None&out_encoding=" ^ font 
            ^ "&parse=Full&text_type=Sloka&mode=web&tlang=Hindi" in 
  anchor Green_ (invoke cgi) check_sign
;
(* Prints segmentation word-forms (without phase details) in list view
   with a link to SCL *)
value print_list_segmentations font id segmentations = do 
  { ps (span_begin Blue_)
  ; ps ((get_sentence font segmentations) ^ " ")
  ; let sentence = (get_sentence "wx" segmentations)
  ; if scl_toggle then 
    call_scl_text sentence scl_font ^ (string_of_int id) |> ps
    else () 
  ; ps span_end
  }
;
(* The following prints the solution on the web page *)
value print_solution text font mode_id fmode_id (id, (_, _, output, all)) = do
  { pl html_break
  ; pl hr
  ; let forget_transitions (_,(phase,word,_)) = (phase,word) in
    let segmentations = List.map forget_transitions output in
    (* If word-frequencies are used, we will not have phase / morph details. 
       So segmentation-forms are listed down with a link to SCL's parser. 
       If morph-frequencies are used, we will have phase and morph details.
       So segmentation-forms with phase and links to morphs are displated, 
       along with a link to SCL's parser. *)
    if word_based_freq freq_mode.val
    then print_list_segmentations font id (List.rev segmentations)
    else if morph_based_freq freq_mode.val
      then print_scl_segments text id mode_id fmode_id (List.rev segmentations)
                              (List.rev output)
    else print_list_segmentations font id (List.rev segmentations)
  }
;

value solution_path = Paths.skt_install_dir ^ "ML/"
;

(* To replace space with '+' *)
value insert_plus strng = 
  let str1 = Str.global_replace (Str.regexp " ") "+" strng in
  (* [let str2 = Str.global_replace (Str.regexp "-") "-+" str1 in] *)
  str1
;
(* To replace "+" with " " *)
value replace_plus str = 
  let temp_str = Str.global_replace (Str.regexp "-\\+") "-" str in 
  Str.global_replace (Str.regexp "+") " " temp_str 
;
(* Print all the solutions into file *)
value print_all_sols cho solutions = 
  loop solutions
  where rec loop = fun
  [ [] -> ()
  | [ item ] -> output_string cho (replace_plus item)
  | [ l :: r ] -> 
    let _ = output_string cho (replace_plus l) in do 
    { output_string cho ";"
    ; loop r 
    }
  ]
;

value get_string (_,str,_,_) = 
  str
;

(* Extract the chunk's segmentation from triplets *)
value get_sandhi_word_for_file (_,_,_,output) = 
  loop "" (List.rev output)
  where rec loop acc = fun
  [ [] -> (String.trim acc)
  | [(_, (phase, rword, transition)) :: tl] -> 
    let prefix = 
      if phase = Unknown then "#"
      else "" in 
    loop (prefix ^ (get_sandhi_word "wx" (phase, rword)) ^ acc) tl
  ]
;
(* Prints the first solution to file in debug mode *)
value print_solution_to_file sols  = 
  let segmented_output_file = solution_path ^ "best_sol.txt" in
    let cho = 
      open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o666 
                   segmented_output_file in do
      { output_string cho (replace_plus (List.hd sols))
      ; flush cho
      ; close_out cho
      }
;

(* To record all the best solutions in debug mode *)
value print_all_sols_to_file wx_input sols = 
  let segmented_output_file = solution_path ^ "best_n_solutions.txt" in
    let cho = 
      open_out_gen [Open_append; Open_creat] 0o666 segmented_output_file in do
      { output_string cho (wx_input ^ "\t"); (* Prints the unsegmented text *)
        (* Prints all solutions separated with ";" *)
        let _ = (print_all_sols cho sols) in 
        output_string cho "\n"
        ;
        flush cho;
        close_out cho
      }
;

(* The following is for recording the best solution(s) to file (for debug) *) 
value print_sols_to_file wx_input mode sols = 
  let solutions = List.map snd sols in 
  let seg_sols = List.map get_sandhi_word_for_file solutions in
  if mode = First_List then let _ = print_solution_to_file seg_sols in ()
  else let _ = print_all_sols_to_file wx_input seg_sols in ()
;

(* Prints solutions on the interface *)
value print_sols text sols font mode fmode_id = (* stats = (kept,max) *) 
  let mode_id = (mode_id_of_mode mode) in 
  let print_sl sls = 
    List.iter (print_solution text font mode_id fmode_id) sls in 
  if mode = First_List then (* To record the first best solution *)
    print_sl [(List.hd sols)]
  else print_sl sols (* To record all the solutions *)
;

(* Adds indices to the solution *)
value add_indices solution_list = 
  loop 0 [] solution_list
  where rec loop id acc = fun
  [ [] -> acc
  | [hd :: tl] -> loop (id + 1) (acc @ [(id + 1, hd)]) tl
  ]
;

(* In Pipeline, when called from sa.msaadhanii's parser, segmented 
   solution and all its possible morph analyses' are directly fed to 
   standard output *)
value write_sol_to_std_out solution_list selected_segments = 
  let final_segments = List.map snd selected_segments
  and seg_sols = List.map get_string solution_list in do 
  { ("sentence: " ^ (replace_plus (List.hd seg_sols))) |> ps
  ; "\nmorph: " |> ps
  ; Scl_parser.post_best_segments_scl final_segments
  }
;

value collapse_duplicates seg_sols = 
  let collapsed_seg_sols = loop [] seg_sols
  where rec loop acc = fun 
  [ [] -> acc
  | [ hd :: tr ] -> if List.mem hd acc then loop acc tr
                    else loop (acc @ [ hd ]) tr
  ] in 
  collapsed_seg_sols
;

(* Get the list of solutions as list string *)
value get_solutions_str solution_list collapse = 
  let seg_solutions = List.map get_string solution_list in 
  let seg_sols = 
    if collapse then collapse_duplicates seg_solutions
    else seg_solutions in 
  let seg_sol_str sol = "\"" ^ (replace_plus sol) ^ "\"" in 
  let seg_sols_lst = List.map seg_sol_str seg_sols in 
  let segmented_sols_str = "[" ^ (String.concat ", " seg_sols_lst) ^ "]" in 
  segmented_sols_str
;

(* In pipeline mode, when the call is from the sandhi-splitter of 
   sa.msaadhanii, then the list view is activated and hence the 
   segmentation solutions alone are writted to stdout as json *)
value write_sol_lst_json_to_stdout solution_list input = 
  let segmented_sols_str = get_solutions_str solution_list False in 
  let solutions_str = "\"segmentation\": " ^ segmented_sols_str
  and input_str = "\"input\": " ^ "\"" ^ input ^ "\"" in 
  let json_string_output = "{" ^ input_str ^ ", " ^ solutions_str ^ "}" in 
  print_string json_string_output
;

value write_sol_lst_json_for_debug solution_list input collapse = 
  let (segmented_sols_str, collapsed_segmented_sols) = 
    if collapse then (get_solutions_str solution_list False, 
                      get_solutions_str solution_list True)
    else (get_solutions_str solution_list False, "[]") in 
  let solutions_str = "\"segmentation\": " ^ segmented_sols_str
  and collapsed_sols_str = 
    "\"collapsed_segmentation\": " ^ collapsed_segmented_sols
  and input_str = "\"input\": " ^ "\"" ^ input ^ "\"" in 
  let json_string_output = 
    "{" ^ input_str ^ ", " ^ solutions_str ^ ", " ^ collapsed_sols_str ^ "}" in 
  print_string json_string_output
;

(* Handle Calls from sa.msaadhanii here (pipeline = t) *)
value handle_scl_calls solution_list selected_segments input mode = 
  match mode with 
  [ Best_Summary | First_Summary -> 
      write_sol_to_std_out solution_list selected_segments 
  | Best_List | First_List -> 
      write_sol_lst_json_to_stdout solution_list input 
  ]
;

(* In Stemmer, segmented form and all its possible morph analyses' 
   are directly fed to standard output *)
value write_json_to_std_out solution_list input selected_segments = 
  let final_segments = List.map snd selected_segments
  and sols_str = get_solutions_str solution_list False in 
  let input_str = "\"input\": " ^ "\"" ^ input ^ "\"" in 
  let word = "\"segmentation\": " ^ sols_str in 
  let morph = "\"morph\": " ^ (get_all_morphs_str final_segments) in 
  let json_string_output = 
    "{" ^ input_str ^ ", " ^ word ^ ", " ^ morph ^ "}" in 
  print_string json_string_output
;
value write_error_json input_str error_str = 
  let input = "\"input\": \"" ^ input_str ^ "\""
  and error = "\"segmentation\": [\"" ^ error_str ^ "\"]" in 
  print_string ("{" ^ input ^ ", " ^ error ^ "}")
;
(* In Debug, segmented solutions are directly output to a local file *)
value write_solutions_to_file wx_input mode solution_list = 
  let numbered_sol_list = add_indices solution_list in 
  let _ = print_sols_to_file wx_input mode numbered_sol_list in 
  let message = "{\"success\": \"Solution(s) written to file\"}" in 
  let _ = print_string message in ()
;
(* The following two functions are introduced for the 
   Best-Summary and Best-List modes. These are accessible from each other.
   Now, the same interface2 has both the modes. *)
(* To display the list of best solutions *)
value display_best_list text deva_input roman_input checkpoints cpts wx_input 
                        undo_enabled font mode solution_list rcheckpoints 
                        fmode = do 
  { html_break |> pl
  ; ps (div_begin Latin16)
  ; html_latin16 "Sentence: " |> pl
  ; match font with
    [ "roma" -> do 
                { ps (span_begin Blue_) 
                ; ps roman_input (* roman *)
                ; ps span_end
                }
    | "deva" -> deva16_blue deva_input |> ps (* devanagari *)
    | _ -> do 
           { ps (span_begin Blue_) 
           ; ps roman_input (* roman for default *)
           ; ps span_end
           }
    ]
  ; table_begin Spacing20 |> pl
  ; tr_begin |> pl (* tr begin *)
  ; let (new_mode, link_text) = 
      if mode = First_List 
        then (mode_id_of_mode First_Summary, "Summary of First Solution") 
      else if mode = Best_List 
        then (mode_id_of_mode Best_Summary, "Summary of Best Solutions") 
      else (mode_id_of_mode Best_Summary, "Summary of Best Solutions") in 
    td_wrap (call_best_graph text new_mode checkpoints 
                             rcheckpoints ^ link_text) |> ps 
  ; td_wrap (call_full_graph text ^ "Summary of All Solutions") |> ps 
  ; tr_end |> pl   (* tr end *)
  ; table_end |> pl (* Spacing20  *)
  ; ps div_end
  ; let numbered_sol_list = add_indices solution_list in 
    print_sols text numbered_sol_list font mode fmode 
  }
;
(* To display the summary of best solutions *)
value display_best_summary text deva_input roman_input checkpoints cpts wx_input 
                           chunks undo_enabled font mode full count solution_list
                           rcheckpoints fmode = do 
  { make_visual cur_chunk.offset
  ; find_conflict 0
  ; html_break |> pl
  ; div_begin Latin16 |> ps
  ; html_latin16 "Sentence: " |> pl
  ; match font with
    [ "roma" -> roma16_blue roman_input |> ps (* romanized *)
    | "deva" -> deva16_blue deva_input |> ps (* devanagari *)
    | _ -> roma16_blue roman_input |> ps (* romanized by default*) 
    ]
  ; html_break |> ps
  ; table_begin Spacing20 |> pl
  ; tr_begin |> pl (* tr begin *)
  ; if undo_enabled then 
       td_wrap (call_undo text (mode_id_of_mode mode) checkpoints 
                          rcheckpoints ^ "Undo") |> ps
    else ()
  ; let (new_mode, link_text) = 
      if mode = First_Summary 
        then (mode_id_of_mode First_List, "First Solution")
      else if mode = Best_Summary 
        then (mode_id_of_mode Best_List, "List of Best Solutions") 
      else (mode_id_of_mode Best_List, "List of Best Solutions") in 
    td_wrap (call_best_list text new_mode checkpoints 
                            rcheckpoints ^ link_text) |> ps
  ; td_wrap (call_full_graph text ^ "Summary of All Solutions") |> ps 
  ; let call_scl_parser () = (* invocation of scl parser *)
        if scl_toggle then
           td_wrap (call_reader text checkpoints rcheckpoints "o" 
                                (mode_id_of_mode mode) fmode 
                    ^ "UoH Analysis Mode") |> ps 
        else () (* [scl_parser] is not visible unless toggle is set *) in
    (* This is a work-around to check whether the number of solutions is 
       equal to 1. When there is only 1 solution, the "Unique Solution" and 
       "Uoh Analysis Mode" are to be active.
       Currently, the segmenter runs only once and registers the graph, 
       also counting the total number of solutions. And then best segments 
       are obtained and the graph is rebuilt. But the count is not changed. 
       In the [First_Summary] mode with stem and morph freqs there will be 
       definitely only one solution. Also, when the segmenter produces only 
       one solution, the count value will be 1. *)
    let single_solution = 
      ((((fmode_of_fmode_id fmode) = Frequency_Stem_Morph) && 
      (mode = First_Summary)) ||
      count = 1) in 
    if single_solution (* Unique remaining solution *) then do
    { td_wrap (call_parser text (checkpoints @ rcheckpoints) 
               ^ "Unique Solution") |> ps
    ; call_scl_parser ()
    } else ()
  ; tr_end |> pl   (* tr end *)
  ; table_end |> pl (* Spacing20  *)
  ; div_end |> ps (* Latin16 *)
  ; html_break |> pl
  ; div_begin Latin12 |> ps
  ; table_begin Tcenter |> pl
  ; tr_begin |> ps
  ; List.iter update_col_length chunks 
  ; if Paths.platform="Station" 
    then print_all text (mode_id_of_mode mode) checkpoints rcheckpoints chunks 0
    else List.iter print_first_server chunks
  ; tr_end |> pl
  ; print_interf text (mode_id_of_mode mode) checkpoints rcheckpoints ()
  ; table_end |> pl
  ; div_end |> ps (* Latin12 *)
  ; html_break |> pl
  } 
;

value best_mode_operations cpts chunks = 
  let _ = do {
    chkpts.all_checks := cpts
  ; chunk_solutions.total_sols := []
  ; chunk_solutions.number_of_chunks := 0
  ; chunk_solutions.possible_splits := []
  ; chunk_solutions.cur_offset := 0
  ; chunk_solutions.solution := ""
  } in
  let (full,count) = segment_iter chunks
  and solution_list = dove_tail chunk_solutions.total_sols in
  (full, count, solution_list)
;
(* The main procedure for computing the graph segmentation structure *)
value check_sentence translit uns text checkpoints input undo_enabled 
      font mode rcheckpoints pipeline stemmer debug fmode =
  let encode = Encode.switch_code translit in
  let chunker = if uns (* sandhi undone *) then Sanskrit.read_raw_sanskrit 
                else (* chunking *) Sanskrit.read_sanskrit in
  let raw_chunks = Sanskrit.read_raw_sanskrit encode input in (* NEW *)
  let chunks = chunker encode input in 
  let deva_chunks = List.map Canon.unidevcode raw_chunks in (* NEW *)
  let roman_chunks = List.map Canon.uniromcode raw_chunks in (* NEW *)
  let deva_input = String.concat " " deva_chunks in 
  let roman_input = String.concat " " roman_chunks in 
  let wx_chunks = List.map Canon.decode_WX raw_chunks in (* NEW *)
  let wx_input = String.concat " " wx_chunks in 
  let rcpts = sort_check (checkpoints @ rcheckpoints) in 
  let cpts = sort_check checkpoints in 
  let (max_solutions, list_mode) = 
    match mode with 
    [ First_Summary | First_List -> (1, First_List)
    | Best_Summary | Best_List -> (default_max_best_solutions, Best_List)
    | _ -> (default_max_best_solutions, mode)
    ] in 
  let (max_sols, collapse) = match fmode with 
  [ "w" | "x" -> (max_solutions, False)
  | "t" | "s" | "m" | "x" | "n" -> (((max_solutions * 2)), True)
  | _ -> (max_solutions, False)
  ] in 
  let _ = max_best_solutions.val := max_sols in 
  let (full,count,solution_list) = 
      best_mode_operations rcpts chunks in (* full iff all chunks segment *) 
  let rebuild = (((List.length rcheckpoints) = 0) && 
                (count >= max_best_solutions.val)) in 
  (* The graph is rebuilt only for the first call. 
     From the second call the checkpoints are used to update the graph *)
  let (selected_segments, updated_rcheckpts) = 
    if rebuild then rebuild_graph solution_list 
    else (get_best_segments solution_list, rcheckpoints) in 
  (* Checkpoints are not sorted here to make sure that 
     the tracking back using Undo is as per the user's selection of segments *)
  let updated_rcpts = updated_rcheckpts in 
  let undo_enabled = ((List.length cpts) > 0) && undo_enabled in 
  (* Pipeline -> is enabled when the Segmenter is accessed directly from
                 sa.msaadhanii. The best segmented solution is directly 
                 fed to the standard output.
     Stemmer -> is enabled for getting the morphological analyses for the
                given word form in the json format
     Debug mode -> is used internally for evaluation purposes 
     By default, these three are disabled to access the usual display of 
     best segments' summary or best solutions' list*)
  if pipeline then handle_scl_calls solution_list selected_segments wx_input mode
  else if stemmer then write_json_to_std_out solution_list wx_input selected_segments 
  (* else if debug then write_solutions_to_file input mode solution_list *)
  (* Instead of saving it in local file, the results are now sent as JSON *)
  else if debug then write_sol_lst_json_for_debug solution_list wx_input collapse
  else
  match mode with 
  [ Best_List | First_List -> 
      display_best_list text deva_input roman_input checkpoints cpts wx_input 
                        undo_enabled font list_mode solution_list updated_rcpts 
                        fmode 
  | Best_Summary | First_Summary -> 
      display_best_summary text deva_input roman_input checkpoints cpts wx_input 
                           chunks undo_enabled font mode full count solution_list
                           updated_rcpts fmode 
  | _ -> raise (Failure ("Incompatible mode")) 
  ]
;
value arguments trans lex font cache st us input topic abs
                corpus_permission corpus_dir sentence_no =
  "t=" ^ trans ^ ";lex=" ^ lex ^ ";font=" ^ font ^ ";cache=" ^ cache ^ 
  ";st=" ^ st ^ ";us=" ^ us ^ ";text=" ^ input ^ 
  ";topic=" ^ topic ^ ";abs=" ^ abs ^ 
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
  { Gen.dump cache Data.public_cache_file (* for [Load_morphs] *)
  ; Gen.dump cachei Data.public_cachei_file (* id *)
  ; Gen.dump auto_cache Data.public_trans_cache_file (* for [Load_transducers] *)
  ; Gen.dump auto_cachei Data.public_trans_cachei_file (* id *)
  }
;
(* We fill gendered entries incrementally in [public_cache_txt_file] *)
value append_cache entry gender =    
  let cho = open_out_gen [ Open_wronly; Open_append; Open_text ] 0o777 
                         Data.public_cache_txt_file in do
  { output_string cho ("[{" ^ entry ^ "}] ({" ^ gender  ^ "})\n")
  ; close_out cho
  }
;
(* Corpus management : saving an analysed sentence in corpus *)
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
  let submit_button_label = Web_corpus.(match corpmode with
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
(* Failsafe Aborting for pipeline, debug, stemmer and default *)
value abort_i lang s1 s2 input p d s = 
  if p (* pipeline *) || d (* debug *) || s (* stemmer *) then 
    (* In some cases, the exception string has double-quotes ("")
       For smooth processing of JSON string, it is replaced with
       single quotation ('') here *)
    let s1_mod = Str.global_replace (Str.regexp "\"") "'" s1 
    and s2_mod = Str.global_replace (Str.regexp "\"") "'" s2 in 
    write_error_json input ("error: " ^ s1_mod ^ " - " ^ s2_mod)
  else abort lang s1 s2
;
(* Main body of sktgraph2 cgi *)
value graph_engine () = 
  let query = Sys.getenv "QUERY_STRING" in
  let env = create_env query in
  (* Multiple environment variables according to modes of use are: 
     text topic st us t lex font cache abs cpts (standard mode) 
     corpdir sentno corpmode (defined in Params) 
     guess gender revised [rev_off] [rev_ind] (User-aid) *)
  let url_encoded_input = get "text" env "" 
  and url_encoded_topic = get "topic" env "" (* topic carry-over *)
  and url_encoded_mode  = get "mode" env "b"
  and fmode = get "fmode" env "w"
  and ppl = get "pipeline" env "f"
  and stm = get "stemmer" env "f"
  and dbg = get "debug" env "f"
  and st = get "st" env "t" (* sentence parse default *)
  and us = get "us" env "f" (* sandhied text default *)
  and translit = get "t" env Paths.default_transliteration (* translit input *)
  and lex = get "lex" env Paths.default_lexicon (* lexicon choice *)
  and font = get "font" env Paths.default_display_font in 
  let ft = font_of_string font (* Deva vs Roma print *)
  and cache = get "cache" env "f" (* no cache default *) in
  (* ft and cache are persistent in the session *)
  let () = toggle_lexicon lex (* sticky lexicon switch *)
  and () = toggle_sanskrit_font ft 
  and () = cache_active.val := cache 
  and abs = get "abs" env "f" (* default local paths *) in 
  let lang = language_of_string lex (* lexicon indexing choice *)
  and input = decode_url url_encoded_input (* unnormalized string *)
  and uns = us="t" (* unsandhied vs sandhied corpus *) 
  and () = if st="f" then Lexer_control.star.val:=False 
           else () (* word vs sentence stemmer *)
  and () = Lexer_control.transducers_ref.val:=Transducers.mk_transducers ()
  and mode = mode_of_mode_id url_encoded_mode
  and pipeline = (ppl = "t")
  and stemmer = (stm = "t")
  and debug = (dbg = "t")
  and url_enc_corpus_permission = (* Corpus mode *)
      get Params.corpus_permission env "true" in 
  let () = assign_frequency fmode in 
  let corpus_permission = 
    url_enc_corpus_permission
    |> decode_url
    |> Web_corpus.permission_of_string in
  let corpus_dir = get Params.corpus_dir env "" 
  and sentence_no = get Params.sentence_no env "" in
  let undo_enabled = sentence_no = "" (* no undo in Reader corpus mode *)
                  || corpus_permission <> Web_corpus.Reader in
  let text = arguments translit lex font cache st us url_encoded_input
                       url_encoded_topic abs url_enc_corpus_permission
                       corpus_dir sentence_no 
  and checkpoints = 
    try let url_encoded_cpts = List.assoc "cpts" env in (* do not use get *)
        parse_cpts (decode_url url_encoded_cpts)
    with [ Not_found -> [] ]
  (* The rejected segments are indicated with the rcheckpoints *)
  and rcheckpoints = 
    try let url_encoded_rcpts = List.assoc "rcpts" env in 
        parse_cpts (decode_url url_encoded_rcpts)
    with [ Not_found -> [] ]
  (* Now we check if cache acquisition is required *)
  and guess_morph = decode_url (get "guess" env "") (* User-aid guessing *)
  and pseudo_gender = decode_url (get "gender" env "") in 
  let _ = if String.length guess_morph > 0 && Paths.platform="Station" then
             (* User-aid cache acquisition *)
             let (entry,gender) = match pseudo_gender with 
                                  [ "" -> parse_guess guess_morph 
                                  | g -> (guess_morph,g) 
                                  ] in do
             { append_cache entry gender
             ; let cache_txt_file = Data.public_cache_txt_file in
               let caches = Nouns.extract_current_caches cache_txt_file in
               make_cache_transducers caches
             }
          else () in
  let revised = decode_url (get "revised" env "") (* User-aid revision *)
  and rev_off = int_of_string (get "rev_off" env "-1") 
  and rev_ind = int_of_string (get "rev_ind" env "-1") in 
  try let _ = 
    match (revised,rev_off,rev_ind) with
    [ ("",-1,-1) -> (* Standard input processing *** Main call *** *) 
      check_sentence translit uns text checkpoints input undo_enabled 
                     font mode rcheckpoints pipeline stemmer debug fmode 
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
      and new_text = arguments translit lex font cache st us updated_input
                               url_encoded_topic abs url_enc_corpus_permission
                               corpus_dir sentence_no 
      and new_input = decode_url updated_input in
      check_sentence translit uns new_text revised_check new_input undo_enabled 
                     font mode rcheckpoints pipeline stemmer debug fmode 
    ] in 
    if (pipeline || stemmer || debug) then ()
    else do 
    {
    (* Rest of the code concerns Corpus mode *)
    (* automatically refreshing the page only if guess parameter *)
      if String.length guess_morph > 0 then 
         ps ("<script>\nwindow.onload = function () {window.location=\"" ^
             graph_cgi2 ^ "?" ^ text ^  
             ";cpts=" ^ (string_points checkpoints) ^ 
             ";rcpts=" ^ (string_points rcheckpoints) ^ "\";}\n</script>")
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
  [ Sys_error s         -> abort_i lang Control.sys_err_mess s (* file pb *) 
                                   url_encoded_input pipeline debug stemmer
  | Stream.Error s      -> abort_i lang Control.stream_err_mess s (* file pb *)
                                   url_encoded_input pipeline debug stemmer
  | Encode.In_error s   -> abort_i lang "Wrong input " s
                                   url_encoded_input pipeline debug stemmer
  | Exit (* Sanskrit *) -> abort_i lang "Wrong character in input" "" 
                                   url_encoded_input pipeline debug stemmer
  | Overflow            -> abort_i lang "Maximum input size exceeded" ""
                                   url_encoded_input pipeline debug stemmer
  | Invalid_argument s  -> abort_i lang Control.fatal_err_mess s (* sub array *)
                                   url_encoded_input pipeline debug stemmer
  | Failure s           -> abort_i lang Control.fatal_err_mess s (* anomaly *)
                                   url_encoded_input pipeline debug stemmer
  | End_of_file         -> abort_i lang Control.fatal_err_mess "EOF" (* EOF *)
                                   url_encoded_input pipeline debug stemmer
  | Not_found           -> let s = "You must choose a parsing option" in
                           abort_i lang "Unset button in form - " s
                                   url_encoded_input pipeline debug stemmer
  | Control.Fatal s     -> abort_i lang Control.fatal_err_mess s (* anomaly *)
                                   url_encoded_input pipeline debug stemmer
  | Control.Anomaly s   -> abort_i lang Control.anomaly_err_mess s
                                   url_encoded_input pipeline debug stemmer
  | _                   -> abort_i lang Control.fatal_err_mess 
                                   "Unexpected anomaly" url_encoded_input
                                   pipeline debug stemmer 
  ]
; 
value safe_engine () =
  (* Problem: in case of error, we lose the current language of the session *)
  let query = Sys.getenv "QUERY_STRING" in
  let env = create_env query in
  let url_encoded_input = get "text" env "" 
  and ppl = get "pipeline" env "f"
  and stm = get "stemmer" env "f"
  and dbg = get "debug" env "f" in
  let pipeline = (ppl = "t")
  and stemmer = (stm = "t")
  and debug = (dbg = "t") in  (* unnormalized string *)
  let _ = if (pipeline || stemmer || debug) then (pl http_header)
          else Prel.prelude () in
  try graph_engine () with  
  [ Failure s -> abort_i default_language Control.fatal_err_mess s 
                         (* [parse_cpts phase_string] ? *)
                         url_encoded_input pipeline stemmer debug
  | _ -> abort_i default_language Control.fatal_err_mess 
                 "Unexpected anomaly - broken session" url_encoded_input 
                 pipeline stemmer debug
  ]
;
end (* Interface2 *)
;
Interface2.safe_engine () (* Should always produce a compliant HTML page *)
;

