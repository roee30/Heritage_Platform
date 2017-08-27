(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2017 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Css = struct i*)

(* Stand-alone module for generating the css file style.css *)

open Html; (* [class_of] style *)
open Web;
 
value cascade (elt,cla) = (* cascading style sheets generator *)
  elt ^ "." ^ (class_of cla) ^ " {" ^ (style cla) ^ "}"
;
value sheets = (* cascading style sheets data *)
  [ ("a",Blue_); ("a",Green_); ("a",Navy_); ("a",Red_)
  ; ("h1",Title); ("h1",C1); ("h2",C2); ("h3",C3)
  ; ("h1",B1); ("h2",B2); ("h3",B3); ("p",G2)
  ; ("div",Latin12); ("div",Latin16); ("div",Enpied); ("div",Center_)
  ; ("span",Alphabet); ("span",Deva); ("span",Trans12); ("span",Devared_)
  ; ("span",Red_); ("span",Roma16o); ("span",Magenta_); ("span",Blue_)
  ; ("span",Green_); ("span",Latin12); ("span",Latin16); ("span",Trans16)
  ; ("span",Title); ("span",C1); ("span",C2); ("span",C3); ("span",Deva20c)
  ; ("span",B1); ("span",B2); ("span",B3); ("span",Header_deva); ("span",Math)
  ; ("span",Devac); ("span",Header_tran); ("span",Deva16); ("span",Deva16c)
  ; ("body",Mauve_back); ("body",Pink_back); ("body",Chamois_back)
  ; ("table",Bandeau); ("table",Center_); ("table",Body); ("table",Pad60) 
  ; ("table",Yellow_back); ("table",Yellow_cent); ("table",Deep_sky_cent)
  ; ("table",Salmon_back); ("table",Aquamarine_back)
  ; ("table",Mauve_back); ("table",Magenta_back); ("table",Mauve_cent)
  ; ("table",Cyan_back); ("table",Cyan_cent); ("table",Lavender_cent)
  ; ("table",Gold_back); ("table",Gold_cent); ("table",Inflexion)
  ; ("table",Chamois_back); ("table",Blue_back) ; ("table",Green_back)
  ; ("table",Brown_back); ("table",Lime_back); ("table",Deep_sky_back)
  ; ("table",Carmin_back); ("table",Orange_back); ("table",Red_back)
  ; ("table",Grey_back); ("table",Pink_back); ("table",Spacing20)
  ; ("table",Light_blue_back); ("table",Lavender_back); ("table",Lawngreen_back)
  ; ("td",Yellow_back); ("td",Yellow_cent); ("td",Deep_sky_cent)
  ; ("td",Salmon_back); ("td",Aquamarine_back)
  ; ("td",Mauve_back); ("td",Magenta_back); ("td",Mauve_cent)
  ; ("td",Cyan_back); ("td",Cyan_cent); ("td",Lavender_cent)
  ; ("td",Gold_back); ("td",Gold_cent); ("td",Inflexion)
  ; ("td",Chamois_back); ("td",Blue_back) ; ("td",Green_back)
  ; ("td",Brown_back); ("td",Lime_back); ("td",Deep_sky_back)
  ; ("td",Carmin_back); ("td",Orange_back); ("td",Red_back)
  ; ("td",Grey_back); ("td",Pink_back); ("td",Spacing20)
  ; ("td",Light_blue_back); ("td",Lavender_back); ("td",Lawngreen_back)
  ; ("th",Cell5); ("th",Cell10); ("th",Border2); ("td",Center_)
  ; ("table",Centered); ("table",Tcenter) ; ("", Hidden_)
  ];

value css_decls =
  [ "a:link {color: Blue}"
  ; "a:visited {color: Purple}"
  ; "a:active {color: Fuchsia}"
  ; "img {border: 0}"
  ; "li " ^ "{" ^ (style B3) ^ "}" (* patch for line numbers in corpus *)
  ] @ List.map cascade sheets 
;
value pop_up_spec =
  "#popBox { position: absolute; z-index: 2; background: " ^ rgb Mauve ^ 
          "; padding: 0.3em; border: none; white-space: nowrap; }"
;
value print_css_file () =
  let output_channel = open_out css_file in  
  let ps = output_string output_channel in
  let pl s = ps (s ^ "\n") in
  let css_style l = List.iter pl l in do
  { css_style css_decls
  ; pl pop_up_spec
  ; close_out output_channel
  }
;
print_css_file ()
;
(*i end; i*)
