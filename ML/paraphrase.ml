(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2020 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* English paraphrase of semantic analysis *)
(* A bit ad-hoc admittedly *)
(*i module Paraphrase = struct i*)
open Skt_morph
  
open Constraints
  
(* [val_of_voice regime root_regime] *)
open Html
  
open Web
  
(* ps pl etc. *)
open Morphology
  
(* [inflexions] *)
let imperative_paraphrase pers num =
  match pers with
  | First ->
      (match num with
       | Singular -> "Let me "
       | Dual -> "Let us two "
       | Plural -> "Let us ")
  | Second ->
      (match num with
       | Singular -> "Thou "
       | Dual -> "You two "
       | Plural -> "You ")
  | Third ->
      (match num with
       | Singular -> "Let it "
       | Dual -> "Let them two "
       | Plural -> "Let them ")
  
let subject_paraphrase pers num =
  match pers with
  | First ->
      (match num with
       | Singular -> "I "
       | Dual -> "Both of us "
       | Plural -> "We ")
  | Second ->
      (match num with
       | Singular -> "Thou "
       | Dual -> "Both of you "
       | Plural -> "You ")
  | Third ->
      (match num with
       | Singular -> "It "
       | Dual -> "Both of them "
       | Plural -> "All of them ")
  
exception Unknown
  
let reg_stem =
  function
  | (* regular english verbs paraphrase *) "k.lp" -> "effect"
  | "krii.d" -> "play"
  | "tan#1" -> "stretch"
  | "tap" -> "suffer"
  | "tyaj" -> "abandon"
  | "dhaav#2" -> "clean"
  | "nind" -> "blame"
  | "pac" -> "cook"
  | "pa.th" -> "learn"
  | "paa#2" -> "protect"
  | "pii" -> "increase"
  | "praz" -> "ask"
  | "bhaa.s" -> "talk"
  | "tarj" | "bharts" -> "threaten"
  | "bruu" -> "say"
  | "bhii#1" -> "fear"
  | "ruc#1" -> "please"
  | "labh" -> "obtain"
  | "lal" -> "fondle"
  | "v.rt#1" -> "exist"
  | "v.r.s" -> "rain"
  | "zubh#1" -> "shine"
  | "sp.rz#1" -> "touch"
  | "svid#2" -> "sweat"
  | "ha#1" -> "kill"
  | _ -> raise Unknown
  
let paraphrase =
  function
  | (* returns pair (present stem, past participle) *)
      "at" | "i" | "gam" | "gaa#1" | "car" -> ("go", "gone")
  | (* irregular verbs *) "as#1" -> ("i", "")
  | "aas#2" | "viz#1" -> ("sit", "seated")
  | "kath" -> ("tell", "told")
  | "j~naa#1" -> ("know", "known")
  | "ta.d" -> ("beat", "beaten")
  | "daa#1" -> ("give", "given")
  | "dhaav#1" -> ("run", "chased")
  | "dh.r" -> ("hold", "held")
  | "nii#1" -> ("lead", "led")
  | "paz" -> ("see", "seen")
  | "paa#1" -> ("drink", "drunk")
  | "bhuj#2" -> ("eat", "eaten")
  | "bhuu#1" -> ("become", "become")
  | "m.r" -> ("die", "dead")
  | "likh" -> ("write", "written")
  | "vac" -> ("speak", "spoken")
  | "vah#1" -> ("carry", "carried")
  | "vid#1" -> ("know", "known")
  | "vid#2" -> ("find", "found")
  | "v.rdh#1" -> ("grow", "grown")
  | "vyadh" | "han" -> ("hit", "hurt")
  | "zru" -> ("hear", "heard")
  | "suu#1" -> ("impel", "impelled")
  | "sthaa#1" -> ("stand", "stood")
  | "svap" -> ("sleep", "asleep")
  | e ->
      (try let regular = reg_stem e in (regular, (regular ^ "ed"))
       with | Unknown -> ("do", "done"))
  
(* default *)
let print_gender =
  function
  | Mas -> ps "[M]"
  | Neu -> ps "[N]"
  | Fem -> ps "[F]"
  | Deictic d ->
      (match d with
       | Speaker -> ps "[Speaker]"
       | (* First person *) Listener -> ps "[Listener]"
       | (* Second person *) Self -> ps "[Self]"
       | (* reflexive subject *) Numeral -> ps "[Num]")
and (* number *) print_number =
  function | Singular -> () | Dual -> ps "(2)" | Plural -> ps "s"
and print_case =
  function
  | Nom -> ps "Subject"
  | (* Actor/Agent *) Acc -> ps "Object"
  | (* Goal *) Voc -> ps "O"
  | (* Invocation *) Ins -> ps "by"
  | (* Agent/Instrument *) Dat -> ps "to"
  | (* Destination *) Abl -> ps "from"
  | (* Origin *) Gen -> ps "of"
  | (* Possessor *) Loc -> ps "in"
and (* Circumstance *) print_person =
  function | First -> ps "I" | Second -> ps "You" | Third -> ()
  
let genitive =
  function | Singular -> "'s" | Dual -> " pair's" | Plural -> "s'"
  
let print_noun c n g =
  match c with
  | Nom | Acc | Voc ->
      ((* direct *) print_case c; print_number n; sp (); print_gender g)
  | Gen -> (print_gender g; ps (genitive n))
  | _ -> ((* oblique *) print_case c; sp (); print_gender g; print_number n)
  
let third_sg act = if (act = "do") || (act = "go") then "es" else "s"
  
let print_role =
  function
  | Subject (_, _) -> ps "Subject"
  | (* Actor/Agent *) Object -> ps "Object"
  | (* Goal/Patient *) Instrument -> ps "Agent"
  | (* Agent/Instrument *) _ -> ()
  
let copula n =
  function
  | First -> if n = Singular then "am" else "are"
  | Second -> "are"
  | Third -> if n = Singular then "is" else "are"
  
let print_verb w f n p =
  let (aspect, demand, _) = regime w f
  and (act, pas) = paraphrase w
  in
    match aspect with
    | Imperfectif ->
        (ps (subject_paraphrase p n);
         if w = "as#1"
         then ps (copula n p)
         else
           (if act = "carry" then ps "carrie" else ps act;
            (match p with
             | First | Second -> ()
             | Third -> if n = Singular then ps (third_sg act) else ()));
         ps " ";
         List.iter print_role demand)
    | Perfectif ->
        (ps (subject_paraphrase p n); ps (copula n p); ps " "; ps pas)
    | Impersonal -> (ps act; ps (third_sg act))
    | Statif -> (ps "It "; ps act; ps (third_sg act))
  
let print_abs entry =
  match root_regime entry with
  | Intransitive | Factitive -> ()
  | _ -> ps "Object"
  
(* conjugation [c] ignored at this stage *)
(* Translation Sanskrit -> English of tool words *)
let translate_tool =
  function
  | "ca" -> "and"
  | "vaa" -> "or"
  | "saha" -> "with"
  | "iva" -> "indeed"
  | "iti" -> "even"
  | "eva" -> "so"
  | "naaman" -> "by name"
  | "yathaa" -> "if"
  | "tathaa" -> "then"
  | x -> x
  
(* keep stem *)
let print_verbal _ = ps "(Participial) "
  
(* TODO *)
(* Adapted from [Morpho.print_morph] with extra string argument [w] for lexeme.
   Called from [Parser.print_roles]. *)
let print_sem w =
  function
  | Noun_form (g, n, c) -> print_noun c n g
  | Part_form (v, g, n, c) -> (print_verbal v; print_noun c n g)
  | Verb_form (f, n, p) -> print_verb w f n p
  | Abs_root _ | Ind_form Abs -> print_abs w
  | Ind_form Adv -> ps "Adverb"
  | Ind_form _ -> ps (translate_tool w)
  | Bare_stem -> ps "Compound"
  | Gati -> ps "Composed"
  | _ -> ()
  
let subj_of p n =
  match p with
  | First ->
      (match n with | Singular -> "I" | Dual -> "Us two" | Plural -> "Us")
  | Second ->
      (match n with
       | Singular -> "Thou"
       | Dual -> "You two"
       | Plural -> "You")
  | Third ->
      (match n with | Singular -> "It" | Dual -> "Both" | Plural -> "They")
  
let print_noun_role =
  function
  | Subject (p, n) -> ps (subj_of p n)
  | Object -> ps "Obj"
  | Instrument -> ps "Agt"
  | Destination -> ps "Dst"
  | Origin -> ps "Org"
  | Possessor -> ps "Pos"
  | Circumstance -> ps "Cir"
  
let print_neg_noun_role k = (ps "-"; print_noun_role k; ps " ")
  
let print_role ((seg, sub, ind), role) =
  (ps
     (html_red
        ((string_of_int seg) ^
           ("." ^ ((string_of_int sub) ^ ("." ^ (string_of_int ind))))));
   ps (html_green " [");
   ps (span_begin Latin12);
   (match role with
    | Process (noun_roles, _) | Subprocess noun_roles ->
        List.iter print_neg_noun_role noun_roles
    | Actor (noun_role, gender, number) ->
        (print_noun_role noun_role;
         (match noun_role with
          | Subject (_, _) -> ()
          | _ -> print_number number);
         print_gender gender)
    | Tool Coordination -> ps " & "
    | Tool _ -> ps " T "
    | Number (_, _, _) -> ps " N "
    | Addressee -> ps " V "
    | Compound -> ps " C "
    | Ignored -> ps " _ ");
   ps span_end;
   ps (html_green "] "))
  

