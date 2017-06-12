(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Constraints machinery *)

open Skt_morph;
open Morphology; (* [inflexions] *)

type noun_role = 
  [ Subject of person and number (* agent of active or patient of passive *)
  | Object (* patient or goal of active or adverb of manner *)
  | Instrument (* agent of passive or instrument of active or adverb of manner *)
  | Destination (* receiver or goal *)
  | Origin (* origin of action or adverb of manner *)
  | Possessor (* dual role as verb complement or noun attribution *)
  | Circumstance (* adverb of time or location *)
  ]
and demand = list noun_role
;
type mood = 
  [ Indicative
  | Imper of bool (* True: Imperative False: Injunctive *)
  ]
;
(* Part of speech *)
type pos =
  [ Process of demand and mood    (* roles governed by a verb form *)
  | Subprocess of demand          (* verbal subphrase *)
  | Actor of noun_role and gender and number (* noun form with morphology *)
  | Addressee                     (* vocative *)
  | Tool of tool                  (* grammatical word *)
  | Compound                      (* iic *)
  | Number of gender and case and number  (* number (gender for eka) *)
  | Ignored                       (* indeclinable not known as tool *)
  ]
(* Combinatorial tools *)
and tool = 
  [ Coordination (* ca *)
  | Post_instrument (* sahaa1 vinaa prep *)
  | Not_Post_instrument (* sahaa1 adv *)
  | Prohibition (* maa *)
  | Post_genitive (* varam *)
  | Todo (* to avoid warning *)
  ]
;
type aspect = 
  [ Imperfectif (* active or middle indicative *)
  | Impersonal  (* intransitive passive *)
  | Perfectif   (* transitive passive *)
  | Statif      (* factitive *)
  ]
;
type regime =
  [ Transitive   (* transitive verbs in active and middle *)
  | Intransitive (* intransitive verbs in active and middle *)
  | Factitive    (* impersonal - no subject *)
  | Quotative    (* aahur - it is said *) 
(*| Bitransitive - use of transitive with 2 accusatives *)
(*| Regime of (list case * list case) - specific regime - unused so far *)
  ]
;
value root_regime : string -> regime
;
(* compute aspect, demand and mood of a verbal finite form *)
value regime : string -> (conjugation * paradigm) -> (aspect * demand * mood)
;
type label = (int * int * int) (* (segment number, homonym index, tag index)     *)
and roles = list (label * pos)
;
value roles_of : int -> list int -> list ((int * list int) * inflexions) -> roles
;
type penalty =
  [ Sentence of (int * int * int * int)
  | Copula of (int * int * int * int * int) 
  | NP of penalty
  ]
;
value eval_penalty : penalty -> int
;
value show_penalty : penalty -> string
;
type  flattening = list (penalty * list roles)
;
value sort_flatten : list roles -> flattening
;
value truncate_groups : flattening -> (flattening * option int)
;
value extract : string -> (label * pos) -> string

; 
