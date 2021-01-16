(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Html = struct i*)

(* Pidgin ML as scripting langage of the poor for HTML and XML *)

(**************************)
(* Generic HTML scripting *)
(**************************)

(* All values are pure, with no side-effect, no printing. *)
(* Attributes given as association lists [(label,value): (string * string)] *)
value assoc_quote (label,valu) = 
  let sp_label = " " ^ label in 
  sp_label ^ "=\"" ^ valu ^ "\""
;
value rec quote_alist = fun
  [ [] -> ""
  | [ assoc_list ] -> assoc_quote assoc_list
  | [ assoc_list :: rest ] -> (assoc_quote assoc_list ^ quote_alist rest)
  ]
;
(* Elementary XML constructors *)
value xml_begin xml_op = "<" ^ xml_op ^ ">" 
and xml_begin_with_att xml_op atts = "<" ^ xml_op ^ (quote_alist atts) ^ ">"
and xml_end xml_op = "</" ^ xml_op ^ ">"
and xml_empty xml_op = "<" ^ xml_op ^ ">" 
and xml_empty_with_att xml_op atts = "<" ^ xml_op ^ (quote_alist atts) ^ ">"
;
value xml_next op = xml_end op ^ xml_begin op
;
value html_break = xml_empty "br"
and html_paragraph = xml_begin "p" ^ xml_end "p"
;
(* Array operations *)
value tr_begin = xml_begin "tr"
and   tr_end   = xml_end "tr"
and   th_begin = xml_begin "th"
and   th_end   = xml_end "th"
and   td_begin = xml_begin "td"
and   td_end   = xml_end "td"
and   td       = xml_empty_with_att "td"
;
value td_wrap text = td_begin ^ text ^ td_end
and cell item = tr_begin ^ th_begin ^ item ^ th_end ^ tr_end
;
(* Dynamic colors depending on mouse position *)
value tr_mouse_begin color_over color_out = 
  xml_begin_with_att "tr" 
          (* beware case of attributes below; colors must be quoted *)
          [ ("onMouseover","this.bgColor=" ^ color_over)
          ; ("onMouseout" ,"this.bgColor=" ^ color_out) 
          ]
;
value input_id = "focus"
and focus_script = (* selection of input window *)
    "(function(){var src = document.getElementById('focus');src.select();})();"
;
value text_area control width length sentence =
  let w = string_of_int width
  and l = string_of_int length in
  xml_begin_with_att "textarea" 
      [ ("id",input_id); ("name",control); ("rows",w); ("cols",l) ] ^ sentence ^
  xml_end "textarea" (* Caution - necessary to separate begin and end *) ^ "\n" ^
  xml_begin_with_att "script" [ ("type","text/javascript") ] ^ focus_script ^ 
  xml_end "script" 
;
(* printing options for the user to choose lemma *)
value option_print id control = 
  xml_begin_with_att "option" [ ("value",id) ] ^ control ^ xml_end "option"
;
value option_print_default id control b = 
  let value_param = ("value",id) in 
  let params = if b then [ value_param; ("selected","selected") ]
                    else [ value_param ] in 
  let menu = xml_begin_with_att "option" params in
  menu ^ control ^ xml_end "option"
;
value rec print_options = fun
   [ [] -> ""
   | [ (id,control) :: rest ] -> option_print id control ^ print_options rest
   ]
;
value rec print_options_default = fun
   [ [] -> ""
   | [ (control,id,b) :: rest ] -> 
       (option_print_default id control b) ^ (print_options_default rest)
   ]
;
value option_select list_options = 
  xml_begin "select" ^ print_options list_options ^ xml_end "select"
;
value option_select_label label list_options = xml_begin_with_att "select" 
  [ ("name",label) ] ^ print_options list_options ^ xml_end "select"
;
value option_select_default label list_options = xml_begin_with_att "select" 
  [ ("name",label) ] ^ print_options_default list_options ^ xml_end "select"
;
value option_select_default_id id label list_options = 
  (xml_begin_with_att "select" [ ("id",id); ("name",label) ]) ^ 
  print_options_default list_options ^ xml_end "select"
;
value text_input id control = 
  xml_empty_with_att "input" [ ("id",id); ("type","text"); ("name",control) ]
;
value add_opt_attrs opt_attrs attrs =
  let add_attr acc (label, v) = 
    match v with
    [ None -> acc
    | Some v -> [ (label, v) :: acc ]
    ] in
  List.fold_left add_attr attrs opt_attrs
;
value int_input ?id ?val ?(step = 1) ?(min = min_int) ?(max = max_int) name =
  let attrs =
    [ ("type", "number")
    ; ("name", name)
    ; ("step", string_of_int step)
    ; ("min", string_of_int min)
    ; ("max", string_of_int max)
    ] in
  let opt_attrs =
    [ ("id", id); ("value", Gen.opt_app string_of_int val) ] in
  let attrs = add_opt_attrs opt_attrs attrs in
  xml_empty_with_att "input" attrs
;
value radio_input control v label = 
  let attrs =  [ ("type","radio"); ("name",control); ("value",v) ] in
  (xml_empty_with_att "input" attrs) ^ label
;
value select control = 
  List.map (fun (label,v) -> radio_input control v label)
;
value radio_input_dft control v label checked = 
  let check = if checked then [ ("checked","checked") ] else [] in 
  let attrs = [ ("type","radio"); ("name",control); ("value",v) ] @ check in
  (xml_empty_with_att "input" attrs) ^ label
;
value select_default name = 
  List.map (fun (label,v,checked) -> radio_input_dft name v label checked)
;
value submit_input label = 
  xml_empty_with_att "input" [ ("type","submit"); ("value",label) ]
;
value reset_input label = 
  xml_empty_with_att "input" [ ("type","reset"); ("value",label) ]
;
value hidden_input name label = 
  xml_empty_with_att "input" [ ("type","hidden"); ("name",name); ("value",label) ]
;

(*********)
(* Lists *)
(*********)

(* List item *)
value li ?id item =
  let li = "li" in
  let attrs = add_opt_attrs [ ("id", id) ] [] in
  xml_begin_with_att li attrs ^ item ^ xml_end li
;
(* Ordered list *)
value ol ?id ?li_id_prefix ?(start = 1) items =
  let ol = "ol" in
  let process i item = 
    let id = let genid prefix = prefix ^ string_of_int (start + i) in
             Gen.opt_app genid li_id_prefix in
    li ?id item in
  let lines = List.mapi process items in 
  let list = String.concat "\n" lines in
  let attrs = add_opt_attrs [ ("id", id) ] [ ("start", string_of_int start) ] in
  xml_begin_with_att ol attrs ^ "\n" ^ list ^ "\n" ^ xml_end ol
;
value fieldn name content = [ ("name",name); ("content",content) ]
and fieldp name content = [ ("property",name); ("content",content) ]
;
type position = [ Left | Center | Right ]
and font_family = list string 
and font_style = [ Normal | Italic | Slanted ]
;
type color = 
  [ Black | White | Red | Blue | Green | Yellow | Orange | Deep_sky | Purple 
  | Grey | Navy | Cyan | Brown | Carmin | Chamois | Broon | Maroon | Aquamarine
  | Gold | Magenta | Mauve | Pink | Gris | Lime | Light_blue | Lavender 
  | Lawngreen | Deep_pink | Pale_rose | Beige | Lilac | Violet ]
;
type basic_style = 
  [ Font_family of font_family  
  | Font_style of font_style 
  | Font_size of int 
  | Textalign of position 
  | Tablecenter
  | Color of color 
  | Bgcolor of color 
  | Position of string 
  | Full_width 
  | Height of int
  | No_margin
  | Border of int
  | Padding of int
  | Cellpadding of int
  | No_border 
  | Border_sep
  | Border_col
  | Border_sp of int
  | Hidden
  ] (* font-weight not supported *)
; 
value rgb = fun (* a few selected HTML colors in rgb data *)  
  [ Black       -> "#000000" 
  | White       -> "#FFFFFF" (* Wheat = "#F0E0B0" ou "#F5DEB3" *) 
  | Red         -> "#FF0000" (* Firebrick = "#B02020" *)
  | Blue        -> "#0000FF" (* Canard = "#0000C0" ou "#0080FF" *)
  | Green       -> "#008000" (* Teal = "#008080" Olive = "#808000" *)
  | Aquamarine  -> "#6FFFC3" (* actually Light Aquamarine *)
  | Lawngreen   -> "#66ff99" (* was "#7CFC00" *)
  | Yellow      -> "#FFFF00" 
  | Orange      -> "#FFA000"  
  | Cyan        -> "#00FFFF" (* Aqua = Cyan, Turquoise = "#40E0D0" *)
  | Purple      -> "#800080" (* Plum = "#E0A0E0" *)
  | Grey        -> "#B8B8B8" (* Slategrey = "#708090" *)
  | Navy        -> "#000080" (* Midnight blue = "#101870" *)
  | Deep_sky    -> "#00C0FF"
  | Brown       -> "#A02820" (* Chocolate = "#D06820" *)
  | Maroon      -> "#800000"
  | Carmin      -> "#FF1975" (* Carmin = "#FF0066" Deep pink= "#FF1090" *) 
  | Chamois     -> "#F5F5DC" (* gris-beige *)
  | Broon       -> "#852B1D" (* good with gold *)
  | Gold        -> "#A58959" (* Silver = "#C0C0C0" *)
  | Magenta     -> "#FF00FF" (* Violet = "#F080F0" Blueviolet = "#8028E0" *)
  | Mauve       -> "#FF99FF" (* Orchid = "#D070D0" *)
  | Pink        -> "#FFC0C0" (* Hotpink = "#FF68B0" Thisle = "#D0C0D0" *)
  | Deep_pink   -> "#FF1493" 
  | Gris        -> "#E2E6E9" (* Salmon = "#F08070" *)
  | Beige       -> "#FFCCA0" 
  | Lime        -> "#00FF00" (* Chartreuse = "#80FF00" *)
  | Lilac       -> "#E6CCFF"
  | Violet      -> "#461B7E"
  | Pale_rose   -> "#FFDDDD" (* Mistyrose = "#FFE4E1" *)
  | Light_blue  -> "#ADD8E6" 
  | Lavender    -> "#E6E6FA" (* or "#E0E8F0" *)
  ]
;
(* quoted color needed for arguments of [tr_mouse_begin] exclusively *)
value color c = "'" ^ rgb c ^ "'"
;
(* Special symbols *)
value check_sign = "&#x2713;" 
and   spade_sign = "&#x2660;"
and   heart_sign = "&#x2661;"
and   x_sign     = "&#10008;"
;
(* Fonts used for the Web site. *)
(* "Times IndUni" is deprecated, now called "IndUni-T" (John Smith's fonts) *)
value roman_fonts = [ "IndUni-T"; "Arial Unicode MS" ] (* "Times_CSX" *)
and greek_fonts = [ "Arial Unicode MS"; "Symbol" ] (* "Latin Extended-B" Greek *)
and diacr_fonts = [ "IndUni-T"; "Arial Unicode MS" ]
   (* Sanskrit transliteration in romanised script with diacritics *)
and deva_fonts = [ "Arial Unicode MS" ] (* Devanagari fonts *)
(* NB: "Devanagari MT" deprecated because wrong rendering of tacchrutvaa *)
; 
value roman_font = Font_family roman_fonts
and   greek_font = Font_family greek_fonts
and   trans_font = Font_family diacr_fonts 
and   deva_font  = Font_family deva_fonts
;
value points  n = string_of_int n ^ "pt"
and   pixels  n = string_of_int n ^ "px"
and   percent n = string_of_int n ^ "%"
;
value font_style = fun
  [ Normal  -> "normal"
  | Italic  -> "italic"
  | Slanted -> "oblique" (* Not well-supported by browsers *)
  ]
;
value justify = fun
  [ Left   -> "left"
  | Center -> "center"
  | Right  -> "right"
  ]
;
(* Style sheet generator *)
value style_sheet = fun 
  [ Font_family fonts -> "font-family: " ^ family 
      where family = String.concat "," fonts  
  | Font_style fs -> "font-style: " ^ font_style fs 
  | Font_size sz -> "font-size:" ^ points sz  
  | Textalign p ->  "text-align:" ^ justify p 
  | Color cl -> "color:" ^ rgb cl 
  | Bgcolor cl -> "background-color:" ^ rgb cl 
  | Position pos -> pos  
  | Tablecenter -> "margin:0 auto"  
  | No_border -> "border: 0" 
  | Border n -> "border: outset " ^ points n 
  | Padding n -> "padding:" ^ points n  
  | Cellpadding p -> "cellpadding:" ^ percent p  
  | Full_width -> "width:100%" 
  | Height h   -> "height:" ^ points h  
  | No_margin -> "margin-left: 0pt; margin-right: 0pt; margin-top: 0pt" 
  | Border_sep -> "border-collapse:separate" 
  | Border_col -> "border-collapse:collapse" 
  | Border_sp n -> "border-spacing:" ^ points n 
  | Hidden -> "display: none"
  ] 
; 
(* Style of enpied bandeau with fixed position at bottom of page - fragile *)
value enpied = "position: fixed; bottom: 0pt; width: 100%"
;
(* All the styles of the various sections - terminology to be streamlined *)
(* NB: When [style_class] is changed, module Css ought to be adapted *)
type style_class = 
    [ Blue_ | Green_ | Navy_ | Red_ | Magenta_ | Hidden_
    | Header_deva | Header_tran | Bandeau | Body | Spacing20 | Pad60 | Border2
    | Latin12 | Trans12 | Deva | Devac | Deva16 | Deva16c | Deva20c 
    | Roma16o | Roma12o | Inflection
    | Alphabet | G2 | Title | Latin16 | Trans16 | Devared_ | Math | Enpied 
    | B1 | B2 | B3 | C1 | C2 | C3 | Cell5 | Cell10 | Center_ | Tcenter | Centered
    | Gold_cent | Mauve_cent | Yellow_cent | Cyan_cent | Deep_sky_cent 
    | Yellow_back | Blue_back | Gris_back | Light_blue_back | Gold_back 
    | Pink_back | Chamois_back | Cyan_back | Brown_back | Lime_back | Grey_back 
    | Deep_sky_back | Carmin_back | Orange_back | Red_back | Mauve_back 
    | Lavender_back | Lavender_cent | Green_back | Lawngreen_back | Magenta_back
    | Aquamarine_back | Gris_cent
    ]
;
value background = fun
    [ Mauve       -> Mauve_back
    | Magenta     -> Magenta_back
    | Pink        -> Pink_back
    | Chamois     -> Chamois_back
    | Yellow      -> Yellow_back
    | Gris        -> Gris_back
    | Cyan        -> Cyan_back
    | Gold        -> Gold_back
    | Brown       -> Brown_back
    | Lime        -> Lime_back
    | Blue        -> Blue_back
    | Light_blue  -> Light_blue_back
    | Deep_sky    -> Deep_sky_back
    | Carmin      -> Carmin_back
    | Orange      -> Orange_back
    | Red         -> Red_back
    | Lavender    -> Lavender_back
    | Green       -> Green_back
    | Lawngreen   -> Lawngreen_back
    | Aquamarine  -> Aquamarine_back
    | Grey        -> Grey_back
    | _ -> failwith "Unknown background style"
    ]
and centered = fun
    [ Mauve    -> Mauve_cent
    | Yellow   -> Yellow_cent
    | Gris     -> Gris_cent
    | Gold     -> Gold_cent
    | Deep_sky -> Deep_sky_cent
    | Cyan     -> Cyan_cent
    | Lavender -> Lavender_cent
    | _ -> failwith "Unknown centered style"
    ]
  ;
(* Table of styles of each style class *)
value styles = fun
    [ Centered        -> [ Tablecenter ]
    | Mauve_cent      -> [ Bgcolor Mauve; Tablecenter; Border 8; Padding 10 ]
    | Gris_cent       -> [ Bgcolor Gris; Tablecenter; Border 5; Padding 10 ]
    | Yellow_cent     -> [ Bgcolor Yellow; Tablecenter; Border 5; Padding 10 ]
    | Lavender_cent   -> [ Bgcolor Lavender; Tablecenter; Border 5; Padding 10 ]
    | Inflection      -> [ Bgcolor Light_blue; Tablecenter; Border 2; Padding 5 ]

    | Deep_sky_cent   -> [ Bgcolor Deep_sky; Tablecenter; Border 5; Padding 10 ]
    | Gold_cent       -> [ Bgcolor Gold; Tablecenter; Border 0; Padding 10 ] 
    | Cyan_cent       -> [ Bgcolor Cyan; Tablecenter; Border 5; Padding 10 ] 
    | Mauve_back      -> [ Bgcolor Mauve ]
    | Magenta_back    -> [ Bgcolor Magenta ]
    | Aquamarine_back -> [ Bgcolor Aquamarine ]
    | Pink_back       -> [ Bgcolor Pale_rose; No_margin ] (* Pink *)
    | Yellow_back     -> [ Bgcolor Yellow ] 
    | Gris_back       -> [ Bgcolor Gris ]
    | Chamois_back    -> [ Bgcolor Chamois; No_margin; Full_width ]
    | Cyan_back       -> [ Bgcolor Cyan ]
    | Gold_back       -> [ Bgcolor Gold ]
    | Brown_back      -> [ Bgcolor Brown; No_margin; Padding 10; Full_width ] 
    | Lime_back       -> [ Bgcolor Lime ]
    | Deep_sky_back   -> [ Bgcolor Deep_sky ]
    | Carmin_back     -> [ Bgcolor Carmin ]
    | Orange_back     -> [ Bgcolor Orange ]
    | Red_back        -> [ Bgcolor Red ]
    | Grey_back       -> [ Bgcolor Grey ]
    | Blue_back       -> [ Bgcolor Blue ]
    | Lawngreen_back  -> [ Bgcolor Lawngreen ]
    | Green_back      -> [ Bgcolor Green ]
    | Light_blue_back -> [ Bgcolor Light_blue ]
    | Lavender_back   -> [ Bgcolor Lavender ]
    | Blue_        -> [ trans_font; Color Blue ] 
    | Green_       -> [ trans_font; Color Green ]
    | Navy_        -> [ trans_font; Color Navy ]
    | Red_         -> [ trans_font; Color Red ]
    | Roma16o      -> [ trans_font; Color Red; Font_size 16; Font_style Slanted ]
    | Devared_     -> [ deva_font; Color Red ]
    | Magenta_     -> [ trans_font; Color Magenta ]
    | Header_deva  -> [ deva_font; Color Red; Font_size 24; Textalign Left ]
    | Header_tran  -> [ trans_font; Color Red; Font_size 24; Textalign Left ]
    | Deva         -> [ deva_font; Color Maroon; Font_size 12 ] 
    | Devac        -> [ deva_font; Color Blue; Font_size 12; Textalign Center ]
    | Deva16       -> [ deva_font; Color Blue; Font_size 16 ] 
    | Deva16c      -> [ deva_font; Color Blue; Font_size 16; Textalign Center ]
    | Deva20c      -> [ deva_font; Color Blue; Font_size 20; Textalign Center ]
    | Alphabet     -> [ trans_font; Font_size 24; Textalign Center ]
    | Title        -> [ roman_font; Color Blue; Font_size 24; Textalign Center ]
    | Trans12      -> [ trans_font; Font_size 12 ]
    | B1           -> [ roman_font; Color Blue; Font_size 20 ]
    | B2           -> [ roman_font; Color Blue; Font_size 16 ]
    | B3           -> [ roman_font; Color Blue; Font_size 12 ]
    | C1           -> [ roman_font; Color Blue; Font_size 20; Textalign Center ]
    | C2           -> [ roman_font; Color Blue; Font_size 16; Textalign Center ]
    | C3           -> [ roman_font; Color Blue; Font_size 12; Textalign Center ]
    | G2           -> [ roman_font; Color Green; Font_size 16 ]
    | Center_      -> [ Textalign Center ]
    | Pad60        -> [ Textalign Center; Height 60; Full_width ]
    | Tcenter      -> [ Tablecenter ]
    | Roma12o      -> [ trans_font; Color Black; Font_size 12; Font_style Slanted ]
    | Latin12      -> [ roman_font; Color Black; Font_size 12 ]
    | Latin16      -> [ roman_font; Color Black; Font_size 16 ]
    | Trans16      -> [ trans_font; Color Black; Font_size 16 ]
    | Math         -> [ greek_font; Color Black; Font_size 12 ]
    | Enpied       -> [ Position enpied ]
    | Bandeau      -> [ roman_font; Bgcolor Cyan; Border_sep; Border_sp 10 
                      ; Full_width ]
    | Body         -> [ roman_font; Bgcolor Pale_rose; Border_sep; Border_sp 10
                      ; Full_width ]
    | Spacing20    -> [ Border_sep; Border_sp 20 ]
    | Cell5        -> [ Padding 5 ]
    | Cell10       -> [ Padding 10 ]
    | Border2      -> [ Border 2 ]
    | Hidden_      -> [ Hidden ]
    ]
;
(* Compiles a class into its style for non-css compliant browsers *)
(* Nowadays mostly used by Css to compile the css style sheet     *)
value style cla = String.concat "; " (List.map style_sheet (styles cla)) 
; 
value class_of = fun
    [ Mauve_cent      -> "mauve_cent"
    | Yellow_cent     -> "yellow_cent"
    | Inflection      -> "inflexion"
    | Deep_sky_cent   -> "deep_sky_cent"
    | Centered        -> "centered"
    | Cyan_cent       -> "cyan_cent"
    | Mauve_back      -> "mauve_back"
    | Magenta_back    -> "magenta_back"
    | Pink_back       -> "pink_back"
    | Gold_cent       -> "gold_cent"
    | Yellow_back     -> "yellow_back"
    | Blue_back       -> "blue_back"
    | Light_blue_back -> "light_blue_back"
    | Gris_back       -> "gris_back"
    | Gris_cent       -> "gris_cent"
    | Chamois_back    -> "chamois_back"
    | Cyan_back       -> "cyan_back"
    | Gold_back       -> "gold_back"
    | Lavender_back   -> "lavender_back"
    | Lavender_cent   -> "lavender_cent"
    | Brown_back      -> "brown_back"
    | Lime_back       -> "lime_back"
    | Deep_sky_back   -> "deep_sky_back"
    | Carmin_back     -> "carmin_back"
    | Orange_back     -> "orange_back"
    | Red_back        -> "red_back"
    | Green_back      -> "green_back"
    | Lawngreen_back  -> "lawngreen_back"
    | Aquamarine_back -> "aquamarine_back"
    | Grey_back       -> "grey_back"
    | Blue_           -> "blue" 
    | Green_          -> "green"
    | Navy_           -> "navy"
    | Red_            -> "red"
    | Roma16o         -> "red16"
    | Roma12o         -> "roma12o"
    | Magenta_        -> "magenta"
    | Header_deva     -> "header_deva"
    | Header_tran     -> "header_tran"
    | Latin12         -> "latin12"
    | Deva            -> "deva"
    | Devared_        -> "devared"
    | Devac           -> "devac" 
    | Deva16          -> "deva16"
    | Deva16c         -> "deva16c"
    | Deva20c         -> "deva20c"
    | Alphabet        -> "alphabet"
    | Title           -> "title"
    | Trans12         -> "trans12"
    | B1              -> "b1"
    | B2              -> "b2"
    | B3              -> "b3"
    | C1              -> "c1"
    | C2              -> "c2"
    | C3              -> "c3"
    | G2              -> "g2"
    | Center_         -> "center"
    | Tcenter         -> "center"
    | Spacing20       -> "spacing20"
    | Latin16         -> "latin16"
    | Trans16         -> "trans16"
    | Math            -> "math"
    | Enpied          -> "enpied"
    | Bandeau         -> "bandeau"
    | Pad60           -> "pad60"
    | Cell5           -> "cell5"
    | Cell10          -> "cell10"
    | Border2         -> "border2"
    | Body            -> "body"
    | Hidden_         -> "hidden"
    ]
; 
(* Allows css style compiling even when browser does not support css *)
(* This support was necessary for Simputer platform - now deprecated *)
value elt_begin_attrs attrs elt cl = 
  let style_attr = (* if Install.css then *) ("class",class_of cl)  
                                 (*  else    ("style",style cl) *) in
  xml_begin_with_att elt [ style_attr :: attrs ] 
;
value elt_begin = elt_begin_attrs []
;
value par_begin = elt_begin "p"
and h1_begin    = elt_begin "h1"
and h2_begin    = elt_begin "h2"
and h3_begin    = elt_begin "h3"
and span_begin  = elt_begin "span"
and span_skt_begin = elt_begin_attrs [ ("lang","sa") ] "span" (* EXP *)
and div_begin   = elt_begin "div"
and body_begin  = elt_begin "body"  
and body_begin_style = elt_begin_attrs margins "body" (* Body margins are null *)
  where margins = [ ("style","margin-left: 0; margin-right: 0; margin-top: 0;") ]
(* Caution: [table_begin_style] is not compliant with HTML5 (dynamic style)    *)
and table_begin_style style attrs = elt_begin_attrs attrs "table" style
and table_begin    = elt_begin "table" 
and td_begin_class = elt_begin "td"
and th_begin_class = elt_begin "th"
and td_begin_att   = xml_begin_with_att "td" (* depr *)
;
value par_end = xml_end "p"
and h1_end    = xml_end "h1"
and h2_end    = xml_end "h2"
and h3_end    = xml_end "h3"
and span_end  = xml_end "span"
and div_end   = xml_end "div"
and body_end  = xml_end "body" 
and table_end = xml_end "table"
;
(* table parameters *)
value noborder = ("border","0")
and nopadding  = ("cellpadding","0%")
and padding5   = ("cellpadding","5%")
and padding10  = ("cellpadding","10%")
and nospacing  = ("cellspacing","0")
and spacing5   = ("cellspacing","5pt")
and spacing20  = ("cellspacing","20pt")
and fullwidth  = ("width","100%")
;
value span style text = span_begin style ^ text ^ span_end
and span_skt style text = span_skt_begin style ^ text ^ span_end
and div style text = div_begin style ^ text ^ div_end
;
(* Centering old style - deprecated *)
value center = div Center_ 
and center_begin = div_begin Center_  
and center_end = div_end
; 
value html_red         = span Red_ 
and html_devared       = span_skt Devared_ 
and html_magenta       = span Magenta_
and html_blue          = span Blue_
and html_green         = span Green_
and html_math          = span Math
and html_trans12       = span Trans12
and html_trans16       = span Trans16
and html_latin12       = span Latin12
and html_latin16       = span Latin16
and roma16_red_sl      = span Roma16o
and roma12_sl          = span Roma12o
and span2_center       = span B2
and span3_center       = span B3
and deva12_blue_center = span_skt Devac
and deva16_blue        = span_skt Deva16
and deva16_blue_center = span_skt Deva16c
and deva20_blue_center = span_skt Deva20c
;
value title s = xml_begin "title" ^ s ^ xml_end "title"
and h1_title s = h1_begin Title ^ s ^ h1_end 
and h1_center s = h1_begin B1 ^ s ^ h1_end 
;
value italics s = xml_begin "i" ^ s ^ xml_end "i" 
and emph s = xml_begin "b" ^ s ^ xml_end "b"
;
value hr = xml_empty "hr"
;
value anchor_ref url link =
  (xml_begin_with_att "a" [ ("href",url) ]) ^ link ^ (xml_end "a")
;
value anchor cl url link =
  (elt_begin_attrs [ ("href",url) ] "a" cl) ^ link ^ (xml_end "a")
;
value anchor_def label link =
  (xml_begin_with_att "a" [ ("name",label) ]) ^ link ^ (xml_end "a")
;
value anchor_define cl label link =
  (elt_begin_attrs [ ("name",label) ] "a" cl) ^ link ^ (xml_end "a")
;
value anchor_graph cl url link =
  "<a href=&quot;" ^ url ^ "&quot;>" ^ link ^ "</a>"
; (* NB: use [&quot;] and not quote sign for Javascript *)
value anchor_begin = xml_begin_with_att "a" [ ("class", "navy") ]
;
value anchor_pseudo url link =
    (xml_begin_with_att "a" [ ("href",url); ("style","text-decoration: none") ])
  ^ link 
  ^ (xml_end "a")
;

(***************************)
(* Specific HTML scripting *)
(***************************)

value start_year = " 1994-"
and current_year = "2021"
and author_name = "Gérard Huet"
;
value copyright = "© " ^ author_name ^ start_year ^ current_year
;
value author = fieldn "author" author_name 
and date_copyrighted = fieldp "dc:datecopyrighted" current_year
and rights_holder = fieldp "dc:rightsholder" author_name
and keywords = fieldn "keywords" 
    "sanskrit,dictionary,heritage,dictionnaire,sanscrit,india,inde,indology,linguistics,panini,digital humanities,digital libraries,cultural heritage,computational linguistics,hypertext lexicon"
;
value heritage_dictionary_title = title "Sanskrit Heritage Dictionary"
;
(* Supported publishing media *)
type medium = [ Html | Tex ]
;
(* Supported HTTP platforms *)
type platform = [ Simputer | Computer | Station | Server ]
;
(* Current target platform to customize - needs recompiling if changed *)
value target = match Paths.platform with
  [ "Simputer" (* Historical - small screen *)
  | "Smartphone" | "Tablet" -> Simputer (* TODO *)
  | "Computer" -> Computer (* Standard client installation *)
  | "Station"  -> Station  (* Permits external Analysis mode and User-aid *) 
  | "Server"   -> Server   (* Http server for Internet web services *)
  | _ -> failwith "Unknown target platform"
  ]
;
(* Features of target architecture *)
value (narrow_screen,screen_char_length,css) = 
 match target with
  [ Simputer -> (True,40,False) (* Historical for Simputer platform *)
  | Station  (* Privileged client mode *)
  | Computer (* Shared frame *)
  | Server   -> (False,80,True) (* Server mode *)
  ]
;
(* Internationalisation *)
type language = [ French | English ] 
;
(* Two indexing lexicons are supported, French SH and English MW.*)
value lexicon_of = fun 
  [ French  -> "SH" (* Sanskrit Heritage *) 
  | English -> "MW" (* Monier-Williams   *) 
  ] 
and language_of_string = fun 
  [ "SH" -> French  
  | "MW" -> English  
  | _ -> failwith "Unknown lexicon" 
  ]
; 
value default_language = Paths.default_lexicon |> language_of_string
;
(* linked lexical resource - initialized at configuration  *)
value lexicon_toggle = ref Paths.default_lexicon (* mutable for lexicon access *)
;
value toggle_lexicon lex = lexicon_toggle.val := lex
; 
value page_extension lang = 
  let lang_sfx = fun  
      [ French  -> ".fr" 
      | English -> ".en" 
      ] in 
  lang_sfx lang ^ ".html"
;
value wrap_ext page lang = page ^ page_extension lang 
;
value site_entry_page = wrap_ext "index"
and dico_index_page   = wrap_ext "index"
and dico_reader_page  = wrap_ext "reader" 
and dico_grammar_page = wrap_ext "grammar" 
and dico_sandhi_page  = wrap_ext "sandhi"  
and dico_corpus_page  = wrap_ext "corpus"
and faq_page          = wrap_ext "faq"  
and portal_page       = wrap_ext "portal"  
;
(* URLs relative to DICO for static pages *)
value rel_dico_path = "../"
;
value images_top_path = "IMAGES/"
;
value rel_sanskrit_page_url l = rel_dico_path ^ (site_entry_page l)
and rel_faq_page_url l        = rel_dico_path ^ (faq_page l)
and rel_portal_page_url l     = rel_dico_path ^ (portal_page l)
and rel_web_images_url        = rel_dico_path ^ images_top_path
;
value rel_image name = rel_web_images_url ^ name
(* rel image is relative in order to pre-compile DICO in distribution site *)
;
value rel_ocaml_logo  = rel_image "icon_ocaml.png" 
and rel_inria_logo    = rel_image "logo_inria.png"
and left_blue_arr     = rel_image "arrw01_16a.gif"
and right_blue_arr    = rel_image "arrw01_06a.gif" 
and rel_favicon       = rel_image "favicon.ico"
;
value meta_prefix = xml_empty_with_att "meta"
;
value contents_instructions = 
  [ [ ("charset","utf-8") ] ]
;
value title_instructions = 
  [ author; date_copyrighted; rights_holder; keywords ]
;
value doctype = "<!DOCTYPE html>" (* Assuming HTML5 *)
;
value url dns = "http://" ^ dns
;
value ocaml_site = url "ocaml.org"
and inria_site = url "www.inria.fr/"
and tomcat = url "localhost:8080/" (* Sanskrit Library runs Tomcat *)
;

(**********)
(* Button *)
(**********)
value js_string_arg s =
  let delim delim s = delim ^ s ^ delim in
  delim "'" s
;
type js_funcall = { js_funid : string; js_funargs : list string }
;
value string_of_js_funcall f =
  let js_funargs = List.map js_string_arg f.js_funargs in
  f.js_funid ^ "(" ^ String.concat ", " js_funargs ^ ")"
;
value button ?id ?cl ?onclick label =
  let button = "button" in
  let attrs = add_opt_attrs
      [ ("onclick", Gen.opt_app string_of_js_funcall onclick)
      ; ("id", id)
      ; ("class", Gen.opt_app class_of cl)
      ] [] in
  let button_begin = xml_begin_with_att button attrs in
  let button_end = xml_end button in
  button_begin ^ label ^ button_end
;
(* Return a copy of the given string with special HTML characters
   represented by escaped sequences (e.g. ['&'] is replaced with
   ["&amp;"] ).  *)
value escape s =
  let conversion_tbl =
    [ ("\"", "quot")
    ; ("&", "amp")
    ; ("'", "apos")
    ; ("<", "lt")
    ; (">", "gt")
    ] in
  let escape s =
    try "&" ^ List.assoc s conversion_tbl ^ ";" with [ Not_found -> s ] in
  let special_chars = Str.regexp 
      ("[" ^ String.concat "" (conversion_tbl |> List.split |> fst) ^ " ]") in
  let subst s = s |> Str.matched_string |> escape in
  Str.global_substitute special_chars subst s
;
