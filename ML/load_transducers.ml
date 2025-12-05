(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)
(* [Load_transducers] *)
(* Used for loading the transducers as well as root informations *)
(* Caution. This is an executable, that actually loads [roots_usage] at
   link time. It also defines a function [load_transducers] that loads 
   [transducers_data] according to parameter [Lexer_control.full],
   and build the relevant [transducter_vect] with [mk_transducers]. 
   All this complication is due to the two modes Simple and Full. 
   The actual transducter vector is stored in [Lexer_control.transducers_ref]
   by a call to [mk_transducers] in [Interface.graph_engine] 
   or [Reader.reader_engine]. *)
open Auto.Auto
  
(* auto State *)
open Automata_vector
  
(* [transducers_datatype] *)
(* There are two different vector of transducers. The raw transducers
   constructed as make time are of type [transducers_datatype] described in
   [Automata_vector]. Then at cgi time the actual vector of transducers 
   indexed by phases is of type [transducer_vect] below. *)
type transducer_vect =
  { nouv : auto; (* vowel-initial nouns *) nouc : auto;
    (* consonant-initial nouns *)
    (*; noun : auto (* declined nouns and undeclinables *) *) pron : auto;
    (* declined pronouns *) root : auto; (* conjugated root forms *)
    (*; krid : auto (* kridantas forms *) *) lopa : auto;
    (* e/o conjugated root forms with lopa *) lopak : auto;
    (* e/o kridantas forms with lopa *) inde : auto;
    (* indeclinables + infinitives *) indifc : auto; (* indeclinables ifc *)
    abso : auto; (* abso-ya *) absv : auto; (* vowel-initial abso-tvaa *)
    absc : auto; (* consonant-initial abso-tvaa *) peri : auto;
    (* periphrastic perfect *) vokv : auto; (* kridanta vocatives *)
    vokc : auto; (* id *) inv : auto; (* invocations *)
    (*; iic  : auto (* iic stems *) *) iifc : auto;
    (* iic forms of ifc stems *) (*; iik  : auto (* iik stems *) *)
    iiv : auto; (* iiv periphrastic stems *) auxi : auto;
    (* as k.r and bhuu finite forms *) auxiinv : auto;
    (* as k.r and bhuu abs and inf forms *) auxik : auto;
    (* their k.r and bhuu kridanta forms supports *) auxiick : auto;
    (* their k.r and bhuu iic kridanta forms supports *)
    (*; ifc  : auto (* ifc forms *) *) ifcv : auto; (* vowel-initial ifc *)
    ifcc : auto; (* consonant-initial ifc *) iiy : auto;
    (* iic avyayiibhava *) avya : auto; (* ifc avyayiibhava *) inftu : auto;
    (* infinitives in -tu *) kama : auto; (* forms of kaama *) vocf : auto;
    (* ifc vocatives *) prev : auto; (* preverb sequences *) pvc : auto;
    (* preverb sequences starting with consonant *) pvv : auto;
    (* preverb sequences starting with vowel *) a : auto; (* privative a *)
    an : auto; (* privative an *) iicv : auto; (* vowel-initial iic *)
    iicc : auto; (* consonant-initial iic *) iivv : auto;
    (* vowel-initial iiv *) iivc : auto; (* consonant-initial iiv *)
    vocv : auto; (* vowel-initial vocatives *) vocc : auto;
    (* consonant-initial vocatives *) iikv : auto; (* vowel-initial iik *)
    iikc : auto; (* consonant-initial iik *) kriv : auto;
    (* vowel-initial krids *) kric : auto; (* consonant-initial krids *)
    cache : auto; (* user-defined supplement to noun *) cachei : auto
  }

(* user-defined supplement to iic *)
let empty_trans = State ((false, [], []))
  
(* dummy empty transducer *)
let dummy_transducer_vect =
  (* needed for initialisation of [transducers_ref]*)
  {
    root = empty_trans;
    pron = empty_trans;
    peri = empty_trans;
    lopa = empty_trans;
    lopak = empty_trans;
    inde = empty_trans;
    indifc = empty_trans;
    abso = empty_trans;
    iifc = empty_trans;
    iiv = empty_trans;
    auxi = empty_trans;
    auxiinv = empty_trans;
    auxik = empty_trans;
    auxiick = empty_trans;
    inv = empty_trans;
    iiy = empty_trans;
    avya = empty_trans;
    inftu = empty_trans;
    kama = empty_trans;
    vocf = empty_trans;
    prev = empty_trans;
    pvc = empty_trans;
    pvv = empty_trans;
    a = empty_trans;
    an = empty_trans;
    iicv = empty_trans;
    iicc = empty_trans;
    ifcv = empty_trans;
    ifcc = empty_trans;
    iivv = empty_trans;
    iivc = empty_trans;
    nouv = empty_trans;
    nouc = empty_trans;
    vocv = empty_trans;
    vocc = empty_trans;
    vokv = empty_trans;
    vokc = empty_trans;
    kriv = empty_trans;
    kric = empty_trans;
    iikv = empty_trans;
    iikc = empty_trans;
    absv = empty_trans;
    absc = empty_trans;
    cache = empty_trans;
    cachei = empty_trans;
  }
  
let abort_load cat =
  let mess = "Missing " ^ (cat ^ " database") in raise (Control.Anomaly mess)
  
module Trans
  (Prel : (* takes its prelude as parameter *)
    sig val prelude : unit -> unit
           end) =
  struct
    let load_transducers () =
      let file = Data.public_transducers_file
      in
        try (Gen.gobble file : transducers_datatype)
        with | _ -> (Prel.prelude (); abort_load "Transducers")
      
    let load_cache () =
      let file = Data.public_trans_cache_file
      in try (Gen.gobble file : auto) with | _ -> empty_trans
    and (* initialised to empty transducer *) load_cachei () =
      let file = Data.public_trans_cachei_file
      in try (Gen.gobble file : auto) with | _ -> empty_trans
      
    (* initialised to empty transducer *)
    (* privative prefixes automata *)
    let a_trans =
      let cch : rule = ([ 22; 23 ], [], [ 23 ])
      in State ((false, [ (1, (State ((true, [], [ cch ])))) ], []))
    and (* a-ch \R acch *) an_trans =
      let n_trans = State ((false, [ (36, (State ((true, [], [])))) ], []))
      in State ((false, [ (1, n_trans) ], []))
      
    (* Splitting an automaton into vowel-initial and consonant-initial solutions *)
    (* with maximum sharing. Assumes deter is in increasing order of phonemes.   *)
    let split deter =
      let (rv, c) =
        let rec split_rec vow con =
          match con with
          | [] -> (vow, [])
          | (((c, _) as arc)) :: rest ->
              if c > 16 then (vow, con) else split_rec (arc :: vow) rest
        in split_rec [] deter
      in ((Word.mirror rv), c)
      
    let split_auto =
      function
      | State ((false, det, [])) ->
          let (vow, con) = split det
          in ((State ((false, vow, []))), (State ((false, con, []))))
      | (* This assumes no non-determinism at the top node *)
          State ((false, det, rules)) ->
          let (vow, con) = split det
          in ((State ((false, vow, rules))), (State ((false, con, []))))
      | (* This assumes non-determinism at the top node, and is needed for the 
       preverb automaton. It assumes that the rules concern the vowel part. *)
          _ -> failwith "Split_auto"
      
    let mk_transducers () = (* : [transducter_vect] *)
      let transducers_data = load_transducers () in
      let transn = transducers_data.nouns and transi = transducers_data.iics
      and transf = transducers_data.ifcs and transk = transducers_data.parts
      and transik = transducers_data.piics
      and transv = transducers_data.vocas and vok = transducers_data.partvocs
      and iiv = transducers_data.iivs and abstvaa = transducers_data.abstvaa
      and pv = transducers_data.preverbs in
      (* now we split the subanta stems and forms starting with vowel or consonant *)
      let (transnv, transnc) = split_auto transn
      and (transiv, transic) = split_auto transi
      and (ifcv, ifcc) = split_auto transf
      and (kriv, kric) = split_auto transk
      and (iikv, iikc) = split_auto transik
      and (iivv, iivc) = split_auto iiv
      and (vocv, vocc) = split_auto transv
      and (vokv, vokc) = split_auto vok
      and (absv, absc) = split_auto abstvaa
      and (pvkv, pvkc) = split_auto pv
      in
        {
          root = transducers_data.roots;
          pron = transducers_data.pronouns;
          peri = transducers_data.peris;
          lopa = transducers_data.lopas;
          lopak = transducers_data.lopaks;
          inde = transducers_data.indecls;
          indifc = transducers_data.indifcs;
          abso = transducers_data.absya;
          iifc = transducers_data.iifcs;
          iiv = iiv;
          auxi = transducers_data.auxis;
          auxiinv = transducers_data.auxiinvs;
          auxik = transducers_data.auxiks;
          auxiick = transducers_data.auxiicks;
          inv = transducers_data.invs;
          iiy = transducers_data.avyayais;
          avya = transducers_data.avyayafs;
          inftu = transducers_data.inftu;
          kama = transducers_data.kama;
          vocf = transducers_data.vocaf;
          prev = pv;
          pvc = pvkc;
          pvv = pvkv;
          a = a_trans;
          an = an_trans;
          iicv = transiv;
          iicc = transic;
          ifcv = ifcv;
          ifcc = ifcc;
          iivv = iivv;
          iivc = iivc;
          nouv = transnv;
          nouc = transnc;
          vocv = vocv;
          vocc = vocc;
          vokv = vokv;
          vokc = vokc;
          kriv = kriv;
          kric = kric;
          iikv = iikv;
          iikc = iikc;
          absv = absv;
          absc = absc;
          cache = load_cache ();
          cachei = load_cachei ();
        }
      
    (* Lexicalized root informations needed for Dispatcher *)
    let roots_usage = (* attested preverb sequences *)
      try (Gen.gobble Data.public_roots_usage_file : string Deco.deco)
      with | _ -> (Prel.prelude (); abort_load "roots_usage")
      
  end
  

