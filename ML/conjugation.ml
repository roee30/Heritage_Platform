(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* CGI-bin conjugation for computing root conjugations.                   *)
(* This CGI is triggered by page [grammar_page] in [dico_dir].            *)
(* Reads its input in shell variable [QUERY_STRING] URI-encoded.          *)
(* Reads its grammatical information from [public_roots_infos_file]       *)
(* Prints an html document of root conjugations on [stdout].              *)
(*i Test: (csh): setenv QUERY_STRING "q=i;c=2"; ./conjugation            i*)
(*i Web: http://skt_server_url/cgi-bin/sktconjug?q=i;c=2                 i*)

(*i module Conjugation = struct i*)

open Skt_morph;
open Morphology; (* inflected [Verb_form] etc. *)
open Conj_infos; (* [vmorph Causa Inten Desid root_infos] *)
open Inflected; (* roots.val indecls.val etc. *)
open Html;
open Web; (* ps pl etc. *)
open Cgi;
open Multilingual; (* font gentense [tense_name] Deva Roma captions *)

value ctitle font = h1_title (conjugation_title narrow_screen font)
and meta_title = title "Sanskrit Grammarian Conjugation Engine"
and back_ground = background Chamois
(* obs [if Install.narrow_screen then background Mauve else Pict_gan] *)
and hyperlink_title font link = 
  if narrow_screen then link
  else conjugation_caption font ^ " " ^ link
;
exception Wrong of string
;

(* For non-unicode compliant browsers replace Canon.uniromcode by Canon.decode *)
value pr code =
  ps (html_red (Canon.uniromcode code) ^ " ") (* roman with diacritics *)
and pr_deva code =
  ps (html_devared (Canon.unidevcode code) ^ " ") (* devanagari *)
;
value pr_f font word =
  let code = Morpho_html.final word in (* visarga correction *)
  match font with
  [ Deva -> pr_deva code
  | Roma -> pr code
  ]
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
value persons_of decls =
  let reorg (one,two,three) (p,form) = match p with
      [ First  -> ([ form :: one ],two,three)
      | Second -> (one,[ form :: two ],three)
      | Third  -> (one,two,[ form :: three ])
      ]
  and init = ([],[],[]) in
  List.fold_left reorg init decls (* (one,two,three) *)
;
value numbers_of l =
  let reorg (sg,du,pl) (n,p,form) = match n with
       [ Singular -> ([ (p,form) :: sg ],du,pl)
       | Dual     -> (sg,[ (p,form) :: du ],pl)
       | Plural   -> (sg,du,[ (p,form) :: pl ])
       ]
   and init = ([],[],[]) in
   List.fold_left reorg init l
;
value acell display s = do
  { ps th_begin
  ; display s
  ; ps th_end
  }
;
value print_row1 caption s d p = do
  { ps tr_begin
  ; acell ps caption
  ; acell ps s
  ; acell ps d
  ; acell ps p
  ; pl tr_end
  }
and print_row_font font caption s d p = 
  let prlist = prlist_font font in do
  { ps (tr_mouse_begin (color Light_blue) (color Pale_yellow))
  ; acell ps caption
  ; acell prlist s
  ; acell prlist d
  ; acell prlist p
  ; pl tr_end 
  }
;
value display font ovoice l =
   let (s,d,p) = numbers_of l in
   let (f1,s1,t1) = persons_of s
   and (f2,s2,t2) = persons_of d
   and (f3,s3,t3) = persons_of p
   and caption = voice_name ovoice font
   and print_row = print_row_font font in do
       { pl html_break
       ; pl (table_begin_style Inflexion [ ("border","2"); padding5 ])
       ; let sing = number_caption Singular font
         and dual = number_caption Dual font 
         and plur = number_caption Plural font in
         print_row1 caption sing dual plur 
       ; match font with
         [ Deva -> do (* Indian style *)
            { print_row (person_name Third Deva)  t1 t2 t3
            ; print_row (person_name Second Deva) s1 s2 s3
            ; print_row (person_name First Deva)  f1 f2 f3
            }
         | Roma -> do (* Western style *)
            { print_row (person_name First Roma)  f1 f2 f3
            ; print_row (person_name Second Roma) s1 s2 s3
            ; print_row (person_name Third Roma)  t1 t2 t3
            }
         ]
       ; ps table_end 
       ; pl html_break
       }
;
value display_table font ovoice = fun
  [ [] -> ()
  | l -> do { ps th_begin; display font ovoice l; ps th_end }
  ]
;
value print_caption font tense = ps (tense_name tense font)
;
value display_amp font otense da dm dp = do 
  { pl (table_begin (centered Mauve))
  ; ps tr_begin
  ; ps th_begin
  ; Gen.optional (print_caption font) otense
  ; pl (xml_begin "table")
  ; ps tr_begin
  ; display_table font Active da 
  ; display_table font Middle dm
  ; display_table font Passive dp
  ; pl tr_end
  ; pl table_end
  ; ps th_end
  ; pl tr_end
  ; pl table_end (* Mauve *)
  }
and display_perfut font pfa = do
  { pl (table_begin_style (centered Mauve) [])
  ; ps tr_begin
  ; ps th_begin
  ; ps (perfut_name font)
  ; pl (xml_begin "table")
  ; ps tr_begin
  ; display_table font Active pfa 
  ; pl tr_end
  ; pl table_end
  ; ps th_end  
  ; pl tr_end
  ; pl table_end (* Mauve *)
  }
;
value sort_out_v accu form = fun
  [ [ (_ (* delta *),morphs) ] -> List.fold_left reorg accu morphs 
      where reorg (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm) = fun 
        [ Verb_form (_(* conj *),te) n p -> let t = (n,p,form) in match te with 
          [ Presenta _ Present -> 
     ([ t :: pa ],pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Presentm _ Present -> 
     (pa,[ t :: pm ],ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Presenta _ Imperfect -> 
     (pa,pm,[ t :: ia ],im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Presentm _ Imperfect -> 
     (pa,pm,ia,[ t :: im ],oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Presenta _ Optative -> 
     (pa,pm,ia,im,[ t :: oa ],om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Presentm _ Optative   -> 
     (pa,pm,ia,im,oa,[ t :: om ],ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Presenta _ Imperative -> 
     (pa,pm,ia,im,oa,om,[ t :: ea ],em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Presentm _ Imperative -> 
     (pa,pm,ia,im,oa,om,ea,[ t :: em ],fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Conjug Future Active -> 
     (pa,pm,ia,im,oa,om,ea,em,[ t :: fa ],fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Conjug Future Middle -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,[ t :: fm ],pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Conjug Perfect Active -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,[ t :: pfa ],pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Conjug Perfect Middle -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,[ t :: pfm ],aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Conjug (Aorist _) Active -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,[ t :: aa ],am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Conjug (Aorist _) Middle | Conjug (Aorist _) Passive -> (* passive-middle *)
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,[ t :: am],ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Conjug (Injunctive _) Active -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,[ t :: ja ],jm,ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Conjug (Injunctive _) Middle | Conjug (Injunctive _) Passive -> (* passive-middle *)
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,[ t :: jm ],ba,bm,fpa,ps,ip,op,ep,ca,cm)
          | Conjug Benedictive Active -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,[ t :: ba ],bm,fpa,ps,ip,op,ep,ca,cm)
          | Conjug Benedictive Middle -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,[ t :: bm ],fpa,ps,ip,op,ep,ca,cm)
          | Perfut Active -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,[ t :: fpa ],ps,ip,op,ep,ca,cm)
          | Presentp Present -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,[ t :: ps ],ip,op,ep,ca,cm)
          | Presentp Imperfect -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,[ t :: ip ],op,ep,ca,cm)
          | Presentp Optative -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,[ t :: op ],ep,ca,cm)
          | Presentp Imperative -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,[ t :: ep ],ca,cm)
          | Conjug Conditional Active -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,[ t :: ca ],cm)
          | Conjug Conditional Middle -> 
     (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,[ t :: cm ])
          | _ -> failwith "Unknown paradigm"
          ]
        | _ -> raise (Control.Fatal "Unexpected verbal form")
        ]
  | _ -> raise (Control.Fatal "Weird inverse map V")
  ]
and init_v = ([],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[])
;
value display_tense3 font tense la lm lp =
   if la=[] && lm=[] && lp= [] then ()
   else match target with
        [ Simputer -> do
          { if la=[] then () else display_amp font (Some tense) la [] []
          ; let caption = if la=[] then Some tense else None in
            if lm=[] then () else display_amp font caption [] lm []
          ; let caption = if la=[] && lm=[] then Some tense else None in
            if lp=[] then () else display_amp font caption [] [] lp
          }
        | _ -> display_amp font (Some tense) la lm lp
        ]
and display_tense2 font tense la lm =
   if la=[] && lm=[] then ()
   else match target with
        [ Simputer -> do
          { if la=[] then () else display_amp font (Some tense) la [] []
          ; let caption = if la=[] then Some tense else None in
            if lm=[] then () else display_amp font caption [] lm []
          }
        | _ -> display_amp font (Some tense) la lm []
        ]
;
value display_conjug font conj = do
  { pl html_paragraph
  ; pl (table_begin (centered Cyan))
  ; ps tr_begin
  ; ps th_begin 
  ; ps (conjugation_name conj font)
  ; ps th_end 
  ; ps tr_end 
  ; pl table_end (* Cyan *)
  ; pl html_paragraph
  }
and display_title font = do
  { pl html_paragraph
  ; pl (table_begin (centered Mauve))
  ; ps tr_begin
  ; ps th_begin 
  ; ps (ctitle font)
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
value display_inflected_v font 
      (pa,pm,ia,im,oa,om,ea,em,fa,fm,pfa,pfm,aa,am,ja,jm,ba,bm,fpa,ps,ip,op,ep,ca,cm) = do
 { pl center_begin 
 ; let tense = Present_tense Present in
   display_tense3 font tense pa pm ps
 ; if ia=[] && im=[] && ip=[] then () else do
     { pl html_break; let tense = Present_tense Imperfect in
                      display_tense3 font tense ia im ip }
 ; if oa=[] && om=[] && op=[] then () else do
     { pl html_break; let tense = Present_tense Optative in
                      display_tense3 font tense oa om op }
 ; if ea=[] && em=[] && ep=[] then () else do
     { pl html_break; let tense = Present_tense Imperative in
                      display_tense3 font tense ea em ep }
 ; if fa=[] && fm=[] then () else do
     { pl html_break; let tense = Other_tense Future in
                      display_tense2 font tense fa fm }
 ; if ca=[] && cm=[] then () else do
     { pl html_break; let tense = Other_tense Conditional in
                      display_tense2 font tense ca cm }
 ; if fpa=[] then () else do
     { pl html_break; display_perfut font fpa }
 ; if pfa=[] && pfm=[] then () else do 
     { pl html_break; let tense = Other_tense Perfect in
                      display_tense2 font tense pfa pfm }
 ; if aa=[] && am=[] then () else do
     { pl html_break; let tense = Other_tense (Aorist 0) in (* forget class *)
                      display_tense2 font tense aa am }
 ; if ja=[] && jm=[] then () else do
     { pl html_break; let tense = Other_tense (Injunctive 0) in (* forget class *)
                      display_tense2 font tense ja jm }
 ; if ba=[] && bm=[] then () else do
     { pl html_break; let tense = Other_tense Benedictive in 
                      display_tense2 font tense ba bm }
 ; pl center_end
 ; pl html_paragraph
 }
;
value display_ind ind font = List.iter disp 
  where disp (_conj,f) = do
  { ps (h3_begin B3)
  ; ps ind
  ; pl html_break
  ; pr_f font f
  ; pl html_break
  ; ps h3_end
  }
;
value display_inflected_u font inf absya per abstva = do
 { pl center_begin 
 ; display_ind (infinitive_caption font) font inf
 ; display_ind (absolutive_caption True font) font abstva
 ; display_ind (absolutive_caption False font) font (List.map prefix_dash absya)
   where prefix_dash (c,w) = (c,[ 0 :: w ]) 
 ; display_ind (peripft_caption font) font per
 ; pl center_end
 }
;
value encode_part = fun
  [ Ppp     -> "Ppp"     
  | Pppa    -> "Pppa"    
  | Ppra _  -> "Ppra"  
  | Pprm _  -> "Pprm" 
  | Pprp    -> "Pprp" 
  | Ppfta   -> "Ppfta" 
  | Ppftm   -> "Ppftm" 
  | Pfuta   -> "Pfuta" 
  | Pfutm   -> "Pfutm" 
  | Pfutp _ -> "Pfutp" 
  | Action_noun -> "Act"
  ]
;
(* inspired from [Print_html.decl_url] *)
value decl_url g s f r part = 
  let (gen,link) = match g with 
     [ Mas -> ("Mas","m.")
     | Neu -> ("Neu","n.")
     | Fem -> ("Fem","f.")
     | _ -> failwith "Unexpected deictic"
     ] in
  let invoke = decls_cgi ^ "?q=" ^ (Transduction.encode_url s)
     ^ ";g=" ^ gen ^ ";font=" ^ f ^ ";r=" ^ (Transduction.encode_url r) 
     ^ ";p=" ^ (encode_part part) (*i [^ ";v=" ^ Install.stamp] - OBS i*)
     ^ ";lex=" ^ lexicon_toggle.val (* Keeping the language *) in
  anchor Red_ invoke link 
;
value display_part font entry part stem_mn stem_f = 
  let str_mn = Canon.decode stem_mn
  and str_f = Canon.decode stem_f 
  and str_font = string_of_font font in do
  { ps (h3_begin B3)
  ; ps (participle_name part font)
  ; pl html_break
  ; pr_f font stem_mn
  ; ps (decl_url Mas str_mn str_font entry part)
  ; ps " "
  ; ps (decl_url Neu str_mn str_font entry part)
  ; ps " "
  ; pr_f font stem_f
  ; ps " "
  ; ps (decl_url Fem str_f str_font entry part)
  ; ps h3_end
  }
;
value abort_display mess = do
  { ps th_end 
  ; ps tr_end 
  ; pl table_end  (* Mauve *)
  ; pl center_end
  ; failwith mess
  }
;
value look_up_and_display font gana entry = 
  let print_conjug conj parts =
  let process_pp = p [] where rec p acc = fun 
  [ [] -> acc 
  | [ x :: rest ] -> match x with 
      [ Parts.Ppp_ con rstem _ when con=conj -> match rstem with
         [ [ 1 :: r ] -> 
           let sm = List.rev rstem 
           and sf = List.rev [ 2 :: r ] in do
           { display_part font entry Ppp sm sf
           ; p acc rest
           }
         | _ -> abort_display "Weird Ppp"
         ]
      | other -> p [ other :: acc ] rest
      ]
  ]
  and process_ppa = p [] where rec p acc = fun 
  [ [] -> acc 
  | [ x :: rest ] -> match x with 
      [ Parts.Pppa_ con stem _ when con=conj -> 
        let sm = Parts.fix stem "at"
        and sf = Parts.fix stem "atii" in do
        { display_part font entry Pppa sm sf
        ; p acc rest
        }
      | other -> p [ other :: acc ] rest
      ]
  ]
  and process_pra = p [] where rec p acc = fun 
  [ [] -> acc 
  | [ x :: rest ] -> match x with 
      [ Parts.Ppra_ k con m_stem f_stem _ when con=conj ->
        let sm = Parts.fix m_stem "at"
        and sf = Parts.fix f_stem "ii" in do
        { display_part font entry (Ppra k) sm sf
        ; p acc rest
        }
      | Parts.Pprared_ con stem _ when con=conj -> 
        let k = if con=Intensive then Parts.int_gana else 3 in
        let sm = Parts.fix stem "at"
        and sf = Parts.fix stem "atii" in do
        { display_part font entry (Ppra k) sm sf
        ; p acc rest
        }
      | other -> p [ other :: acc ] rest
      ]
  ] 
  and process_prm = p [] where rec p acc = fun 
  [ [] -> acc 
  | [ x :: rest ] -> match x with 
      [ Parts.Pprm_ k con stem _ when con=conj -> 
        let sm = List.rev [ 1 :: stem ]
        and sf = List.rev [ 2 :: stem ] in do
        { display_part font entry (Pprm k) sm sf
        ; p acc rest
        }
      | other -> p [ other :: acc ] rest
      ]
  ] 
  and process_prp = p [] where rec p acc = fun 
  [ [] -> acc 
  | [ x :: rest ] -> match x with 
      [ Parts.Pprp_ con stem _ when con=conj -> 
        let sm = List.rev [ 1 :: stem ]
        and sf = List.rev [ 2 :: stem ] in do
        { display_part font entry Pprp sm sf
        ; p acc rest
        }
      | other -> p [ other :: acc ] rest
      ]
  ] 
  and process_pfta = p [] where rec p acc = fun 
  [ [] -> acc 
  | [ x :: rest ] -> match x with 
      [ Parts.Ppfta_ con stem _ when con=conj -> 
        let vstem = if Phonetics.monosyllabic stem then
                       if stem = [ 34; 3; 45 ] (* vid *) then stem (* should test entry *)
                       else List.rev (Parts.fix stem "i") (* intercalating i *)
                    else stem in
        let sm = Parts.fix vstem "vas"
        and sf = Parts.fix stem "u.sii" in do
        { display_part font entry Ppfta sm sf
        ; if con=Primary && 
              (  stem = [ 34; 3; 45 ] (* vid *) (* [Parts.build_more_ppfa] *)
              || stem = [ 46; 3; 45; 3; 45 ] (* vivi's *) (* horrible code *)
              || stem = [ 46; 7; 34; 1; 34 ] (* dad.r's *) 
              )
          then let sm = Parts.fix vstem "ivas" in 
               display_part font entry Ppfta sm sf
          else ()
        ; p acc rest
        }
      | other -> p [ other :: acc ] rest
      ]
  ] 
  and process_pftm = p [] where rec p acc = fun 
  [ [] -> acc 
  | [ x :: rest ] -> match x with 
      [ Parts.Ppftm_ con stem _ when con=conj -> 
        let sm = List.rev [ 1 :: stem ]
        and sf = List.rev [ 2 :: stem ] in do
        { display_part font entry Ppftm sm sf
        ; p acc rest
        }
      | other -> p [ other :: acc ] rest
      ]
  ] 
  and process_futa = p [] where rec p acc = fun 
  [ [] -> acc 
  | [ x :: rest ] -> match x with 
      [ Parts.Pfuta_ con stem _ when con=conj -> 
        let sm = Parts.fix stem "at" 
        and sf = Parts.fix stem "antii" in do
        { display_part font entry Pfuta sm sf
        ; p acc rest
        }
      | other -> p [ other :: acc ] rest
      ]
  ] 
  and process_futm = p [] where rec p acc = fun 
  [ [] -> acc 
  | [ x :: rest ] -> match x with 
      [ Parts.Pfutm_ con stem _ when con=conj -> 
        let sm = List.rev [ 1 :: stem ]
        and sf = List.rev [ 2 :: stem ] in do
        { display_part font entry Pfutm sm sf
        ; p acc rest
        }
      | other -> p [ other :: acc ] rest
      ]
  ] 
  and process_pfp = p [] where rec p acc = fun 
  [ [] -> acc 
  | [ x :: rest ] -> match x with 
      [ Parts.Pfutp_ con rstem _ when con=conj -> match rstem with
         [ [ 1 :: r ] -> 
           let k = match r with
              [ [ 42 :: [ 45 :: [ 1 :: [ 32 :: _ ] ] ] ] -> 3 (* -tavya *)
              | [ 42 :: [ 4 :: _ ] ] -> 2 (* -aniiya *)
              | [ 42 :: _ ] -> 1 (* -ya *)
              | _ -> failwith ("Weird Pfp: " ^ Canon.rdecode rstem)
              ] in 
           let sm = List.rev rstem 
           and sf = List.rev [ 2 :: r ] in do
           { display_part font entry (Pfutp k) sm sf
           ; p acc rest
           }
         | _ -> failwith "Weird Pfutp"
         ]
      | other -> p [ other :: acc ] rest
      ]
  ] 
  and sort_out_u accu form = fun
  [ [ (_,morphs) ] -> List.fold_left (reorg form) accu morphs
      where reorg f (inf,absya,per,abstva) = fun
        [ Ind_verb (c,Infi) when c=conj -> ([ (c,f) :: inf ],absya,per,abstva) 
        | Ind_verb (c,Absoya) when c=conj -> (inf,[ (c,f) :: absya ],per,abstva) 
        | Ind_verb (c,Perpft) when c=conj -> (inf,absya,[ (c,f) :: per ],abstva) 
        | Abs_root c when c=conj -> (inf,absya,per,[ (c,f) :: abstva ]) 
        | _ -> (inf,absya,per,abstva)
        ]
  | _ -> raise (Control.Fatal "Weird inverse map N")
  ]
  and init_u = ([],[],[],[])
  and buckets = Deco.fold sort_out_v init_v roots.val in do 
  (* Main [print_conjug] *)
      { display_conjug font conj
      ; display_inflected_v font buckets (* Display finite root forms *)
      ; pl html_paragraph
      ; pl center_begin (* Now display participial root forms *)
      ; pl (table_begin_style (centered Mauve) [])
      ; ps tr_begin 
      ; ps th_begin 
      ; ps (participles_caption font)
      ; let rest = process_pp parts  in (* Past Passive *)
        let rest = process_ppa rest  in (* Past Active *)
        let rest = process_pra rest  in (* Present Active *)
        let rest = process_prm rest  in (* Present Middle *)
        let rest = process_prp rest  in (* Present Passive *)
        let rest = process_futa rest in (* Future Active *)
        let rest = process_futm rest in (* Future Middle *)
        let rest = process_pfp rest  in (* Future Passive = gerundive *)
        let rest = process_pfta rest in (* Perfect Active *)
        let _ = process_pftm rest    in (* Perfect Middle *) do
          { ps th_end 
          ; ps tr_end 
          ; pl table_end  (* Mauve *)
          ; pl center_end
          ; pl html_paragraph (* Now display indeclinable root forms if any *)
          ; let (inf,_,_,abstvaa) = Deco.fold sort_out_u init_u abstvaa.val 
            and (_,absya,_,_)   = Deco.fold sort_out_u init_u absya.val
            and (_,_,per,_)     = Deco.fold sort_out_u init_u peri.val in
            if absya=[] && per=[] && abstvaa=[] then () else do
            (* Display indeclinable forms *)
            { pl center_begin 
            ; pl (table_begin_style (centered Mauve) [])
            ; ps tr_begin
            ; ps th_begin
            ; ps (indeclinables_caption font)
            ; display_inflected_u font inf absya per abstvaa 
            ; ps th_end
            ; ps tr_end
            ; pl table_end (* Mauve *)
            ; pl center_end
            }
          } 
     } (* end [print_conjug] *) in  
    let compute_conjugs = List.iter (Verbs.compute_conjugs_stems entry) in
    let secondary_conjugs infos =
      (*i TO DO : fix for factorization of aa flag below i*)
      let cau_filter = fun [ (Causa _,_) -> True | _ -> False ]
      and int_filter = fun [ (Inten _,_) -> True | _ -> False ]
      and des_filter = fun [ (Desid _,_) -> True | _ -> False ] in do
      { let causatives = List.filter cau_filter infos in 
        if causatives = [] then () else do 
          { roots.val := Deco.empty 
          ; compute_conjugs causatives
          ; print_conjug Causative Parts.participles.val
          } 
      ; let intensives = List.filter int_filter infos in 
        if intensives = [] then () else do
          { roots.val := Deco.empty 
          ; compute_conjugs intensives
          ; print_conjug Intensive Parts.participles.val 
          } 
      ; let desideratives = List.filter des_filter infos in
        if desideratives = [] then () else do
          { roots.val := Deco.empty 
          ; compute_conjugs desideratives
          ; print_conjug Desiderative Parts.participles.val
          } 
      } in do 
   (* Main [look_up_and_display] *)
   { Verbs.fake_compute_conjugs gana entry (* builds temporaries roots.val etc *)
   ; let infos = (* should be a call to a service that gives one [entry_infos] *)
     (Gen.gobble public_roots_infos_file : Deco.deco root_infos) in
     let entry_infos = Deco.assoc (Encode.code_string entry) infos in 
     if gana = 0 then secondary_conjugs entry_infos 
     else print_conjug Primary Parts.participles.val 
   }
;
value in_lexicon entry = (* entry as a string in VH transliteration *)
  Index.is_in_lexicon (Encode.code_string entry)
and doubt s = "?" ^ s
; 
(* Compute homonym index for a given present class. *)
(* This is very fragile: lexicon update induces code adaptation. *)
(* Temporary - should be subsumed by unique naming structure. *)
value resolve_homonym entry = 
  let first e = e ^ "#1"
  and second e = e ^ "#2"
  and third e = e ^ "#3"
  and fourth e = e ^ "#4" in fun
  [ 1 -> match entry with 
    [ ".rc"
    | "krudh"
    | "kha~nj" 
    | "cit" 
    | "chad" 
    | "tyaj" 
    | "tvi.s" 
    | "dah"
    | "daa" (* ambiguous with ["daa#3"] *)
    | "diz" 
    | "d.rz"
    | "dyut"
    | "dru"
    | "dhaa"
    | "dhaav" (* ambiguous with ["dhaav#2"] *)
    | "nii"
    | "pat"
    | "paa" (* ambiguous with ["paa#2"] *)
    | "budh"
    | "b.rh"
    | "bhuu"
    | "m.rd"
    | "mud"
    | "yaj"
    | "yat"
    | "raaj"
    | "ruc"
    | "rud"
    | "rudh" 
    | "vas"
    | "vah"
    | "v.r"
    | "v.rdh"
    | "vi.s"
    | "zii" 
    | "zuc" 
    | "zubh" 
    | "zcut" 
    | "sad"
    | "sah"
    | "suu" 
    | "sthaa" 
    | "snih" 
    | "spaz" (* no present *)
    | "h.r" -> first entry
    | "gaa"
    | "yu" 
    | "vap" (* ambiguous with [vap#1] *)
    | "sidh" 
    | "svid" -> second entry
    | "maa" -> fourth entry
    | "arc" -> ".rc#1" (* link - bizarre *)
    | _ -> entry
    ]
  | 2 -> match entry with 
    [ "ad"
    | "as"
    | "iiz"
    | "duh"
    | "draa" (* ambiguous with ["draa#2"] *)
    | "dvi.s" 
    | "praa"
    | "praa.n"
    | "bhaa"
    | "maa"
    | "yaa"
    | "yu"
    | "raa"
    | "rud"
    | "lih"
    | "vid" (* ambiguous with ["vid#2"] *)
    | "vii" 
    | "zii" 
    | "zvas"
    | "suu"
    | "han" -> first entry
    | "an" 
    | "aas"
    | "daa"
    | "paa"
    | "vas" 
    | "vaa" -> second entry
    | "nii" -> third entry
    | _ -> entry
    ]
  | 3 -> match entry with 
    [ "gaa" 
    | "daa"
    | "dhaa"
    | "p.r"
    | "bhii"
    | "maa" (* ambiguous with ["maa#3"] *)
    | "vi.s"
    | "haa" -> first entry (* ambiguous with ["haa#2"] used in middle *)
    | "yu" -> second entry
    | _ -> entry
    ]
  | 4 -> match entry with 
    [ "k.sudh" 
    | "dam"
    | "diiv"
    | "d.rz"
    | "druh"
    | "dhii" 
    | "naz" 
    | "pu.s"
    | "budh"
    | "mad" 
    | "yudh"
    | "zam" 
    | "saa"
    | "sidh" 
    | "snih" 
    | "snuh" -> first entry
    | "as" 
    | "i.s"
    | "tan"
    | "daa"
    | "draa" 
    | "dhaa"
    | "pat"
    | "svid" -> second entry
    | "vaa" -> third entry
    | _ -> entry
    ]
  | 5 -> match entry with 
    [ "az" 
    | "k.r"
    | "dhuu"
    | "v.r" -> first entry
    | "su" 
    | "hi" -> second entry
    | _ -> entry
    ]
  | 6 -> match entry with 
    [ "i.s" 
    | "k.rt"
    | "g.rr"
    | "tud"
    | "diz"
    | "d.r"
    | "pi"
    | "bhuj"
    | "muc"
    | "yu"
    | "rud" 
    | "viz" 
    | "suu" 
    | "s.rj"
    | "sp.rz" 
      -> first entry
    | "p.r" 
    | "b.rh" 
    | "rudh" 
    | "vid" (* ambiguous with ["vid#1"] *) 
      -> second entry 
    | "vas" -> fourth entry
    | _ -> entry
    ]
  | 7 -> match entry with 
    [ "chid"
    | "bhid"
    | "yuj" -> first entry
    | "bhuj" 
    | "rudh" -> second entry
    | _ -> entry
    ]
  | 8 -> match entry with 
    [ "k.r"
    | "tan" 
    | "san" -> first entry
    | _ -> entry
    ]
  | 9 -> match entry with 
    [ "az" 
    | "g.rr" 
    | "j~naa" 
    | "jyaa" 
    | "puu"
    | "m.rd" 
    | "luu" -> first entry
    | "v.r" 
    | "h.r" -> second entry
    | _ -> entry
    ] 
  | 10 -> entry
  | 11 -> match entry with 
    [ "mahii" -> second entry
    | _ -> entry
    ]
   | 0 -> if in_lexicon entry (* ad-hoc disambiguation for secondary conjugs *)
             then entry
          else let fentry = first entry in 
               if in_lexicon fentry then fentry else raise (Wrong entry)
  | _ -> ""
  ]
;
value conjs_engine () = do
  { pl http_header
  ; page_begin meta_title 
  ; pl (body_begin back_ground)
  ; let query = Sys.getenv "QUERY_STRING" in
    let env = create_env query in
   try
    let url_encoded_entry = List.assoc "q" env
    and url_encoded_class = List.assoc "c" env 
    and font = font_of_string (get "font" env Paths.default_display_font)
    (* OBS and stamp = get "v" env "" *)
    and translit = get "t" env "VH" (* DICO created in VH trans *)
    and lex = get "lex" env "SH" (* default Heritage *) in 
    let entry_tr = decode_url url_encoded_entry (* : string in translit *)
    and lang = language_of lex 
    and gana = match decode_url url_encoded_class with
      [ "1" -> 1 
      | "2" -> 2
      | "3" -> 3
      | "4" -> 4
      | "5" -> 5
      | "6" -> 6
      | "7" -> 7
      | "8" -> 8
      | "9" -> 9
      | "10" -> 10
      | "11" -> 11 (* denominative verbs *)
      | "0" -> 0 (* secondary conjugations *)
      | s -> raise (Control.Fatal ("Weird present class: " ^ s)) 
      ] 
    and encoding_function = Encode.switch_code translit 
    and () = toggle_lexicon lex in
    try let word = encoding_function entry_tr in
        let entry_VH = Canon.decode word in (* ugly detour via VH string *)  
        (* Beware - 46 decodes as "z" and 21 as "f" *)
        let entry = resolve_homonym entry_VH gana in (* VH string with homo *)
        let known = in_lexicon entry (* in lexicon? *) 
          (* we should check it is indeed a root or denominative *) in do 
        { display_title font 
        ; let link = if known then Morpho_html.skt_anchor False font entry 
                     else doubt (Morpho_html.skt_roma entry) in 
          let subtitle = hyperlink_title font link in
          display_subtitle (h1_center subtitle)
        ; try look_up_and_display font gana entry 
          with [ Stream.Error s -> raise (Wrong s) ]
        ; page_end lang True
        } 
    with [ Stream.Error _ -> 
             abort lang ("Illegal " ^ translit ^ " transliteration ") entry_tr ]
   with [ Not_found -> failwith "parameters q or c missing" ]
  }
;
value safe_engine () =
  let abor = abort default_language in
  try conjs_engine () with 
  [ Sys_error s         -> abor Control.sys_err_mess s (* file pb *)
  | Stream.Error s      -> abor Control.stream_err_mess s (* file pb *)
  | Invalid_argument s  -> abor Control.fatal_err_mess s (* sub *)
  | Wrong s             -> abor "Error - wrong root or class ? - " s
  | Failure s           -> abor "Wrong_input ? " s 
  | Control.Fatal s     -> abor Control.fatal_err_mess s (* anomaly *)
  | Not_found           -> abor Control.fatal_err_mess "assoc" (* assoc *)
  | Control.Anomaly s   -> abor Control.fatal_err_mess ("Anomaly: " ^ s)
  | End_of_file         -> abor Control.fatal_err_mess "EOF" (* EOF *)
  | Encode.In_error s   -> abor "Wrong_input " s
  | Exit (* Sanskrit *) -> abor "Wrong character in input - " "use ASCII" 
  | _                   -> abor Control.fatal_err_mess "anomaly" (* ? *) 
  ]
;
safe_engine () (* Should always produce a legal xhtml page *)
;

(*i end; i*)
