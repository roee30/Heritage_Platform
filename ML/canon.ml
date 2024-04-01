(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2019 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Canon = struct i*)

(* Inverse of [Transduction.code_raw] - word to VH transliteration *)
(* Except that .ll has no canonical code *)
value canon = fun 
  [ 0  -> "-" (* notation for suffixes and segmentation hint in compounds *)
  | 1  -> "a"
  | 2  -> "aa"
  | 3  -> "i"
  | 4  -> "ii"
  | 5  -> "u"
  | 6  -> "uu"
  | 7  -> ".r"
  | 8  -> ".rr"
  | 9  -> ".l"
  | 10 -> "e"
  | 11 -> "ai"
  | 12 -> "o"
  | 13 -> "au"
  | 14 -> ".m" (* anusvaara *)
  | 15 -> "~~" (* anun\=asika candrabindu *)
  | 16 -> ".h"
  | 17 -> "k"
  | 18 -> "kh"
  | 19 -> "g"
  | 20 -> "gh"
  | 21 -> "f" (* used to be "\"n" -- fragile *)
  | 22 -> "c"
  | 23 -> "ch"
  | 24 -> "j"
  | 25 -> "jh"
  | 26 -> "~n"
  | 27 -> ".t"
  | 28 -> ".th"
  | 29 -> ".d"
  | 30 -> ".dh"
  | 31 -> ".n"
  | 32 -> "t"
  | 33 -> "th"
  | 34 -> "d"
  | 35 -> "dh"
  | 36 -> "n"
  | 37 -> "p"
  | 38 -> "ph"
  | 39 -> "b"
  | 40 -> "bh"
  | 41 -> "m"
  | 42 -> "y"
  | 43 -> "r"
  | 44 -> "l" (* Vedic l not accommodated *)
  | 45 -> "v"
  | 46 -> "z" (* used to be "\"s" -- fragile *)
  | 47 -> ".s"
  | 48 -> "s"
  | 49 -> "h" (* h/.dh *)
  | 50 -> "_" (* hiatus *)
  | -1 -> "'" (* avagraha *)
  | -2 -> "[-]" (* amuissement - lopa of a or aa in preceding preverb *)
  | -3 -> "aa|a" (* sandhi of aa and a *a *)
  | -4 -> "aa|i" (* sandhi of aa and i *i *)
  | -5 -> "aa|u" (* sandhi of aa and u *u *)
  | -6 -> "aa|r" (* sandhi of aa and .r *r *)
  | -7 -> "aa|I" (* sandhi of aa and ii *I *)
  | -8 -> "aa|U" (* sandhi of aa and uu *U *)
  | -9 -> "aa|A" (* sandhi of aa and aa *A *)
  | 123 -> "aa|C" (* sandhi of aa and ch *C for ch gemination in cch *)
  | 100 -> "+" (* notation for segmentation hint *)
  | 124 -> failwith "Canon: Unrestored special phoneme j'"  (* j/z *)
  | 149 -> failwith "Canon: Unrestored special phoneme h'"  (* h/gh *)
  | 249 -> failwith "Canon: Unrestored special phoneme h''" (* h/dh *)
  | n -> if n<0 || n>59 then failwith mess 
            where mess = "Canon: Illegal char " ^ string_of_int n
         else "#" ^ Char.escaped (Char.chr (n-2)) (* homo index 1 to 9 *)
                                 (* n-2 above since (ASCII) Char.chr 48 = '0' *)

  ]
;
(* Hiatus-conscious catenation [b=True] iff [s] starts with vowel *)
value catenate c (s,b) = 
  let b'= c>0 && c<14  (* Phonetics.vowel c *) in
  let protected = if b && b' then "_" ^ s else s in
  (canon c ^ protected , b')
;
(* [decode : word -> string] *)
value decode word = 
  let (s,_) = List.fold_right catenate word ("",False) in s 
;
value robust_decode c = (* used in [Morpho_tex.print_inverse_map_txt] *)
  let render n = 
    try canon n with
    [ Failure _ -> match n with
       [ 124 -> "j'" | 149 -> "h'" | 249 -> "h''"
       | _ -> string_of_int n 
       ]
    ] in 
  let conc s s' = render s ^ s' in
  List.fold_right conc c "" (* note no hiatus computation *)
;
(*i Debug when ordinary decode fails 
value decode w = try decode w with 
   [ Failure _ -> failwith (robust_decode w) ]
; i*)

value rdecode w = decode (Word.mirror w)
;
(**************************************************************************)
(* Important information for corpus processing                            *)
(**************************************************************************)
(* Beware. [decode (code_raw s)] is [s] with spaces removed 
   but [code_raw (decode c)] may not be [c] because of VH ambiguities such as 
      [decode [1;3] = decode [11] = "ai"].
   Note that unsandhied text with spaces is wrongly parsed:
      [code_raw "a i" =  [11]] and not [[1; 50; 3]]. 
   Thus one should use underscore for hiatus in digitalised corpus: 
      [code_raw "a_i" =  [1; 3]]. The chunking of text by interpreting spaces 
   is done in a preliminary pass by Chunker.chunker. *)

(* Support for other translitteration schemes *)

(* Wax decoding - University of Hyderabad *)
value canon_WX = fun 
  [ 0  -> "-"
  | 1  -> "a"
  | 2  -> "A"
  | 3  -> "i"
  | 4  -> "I"
  | 5  -> "u"
  | 6  -> "U"
  | 7  -> "q"
  | 8  -> "Q"
  | 9  -> "L"
  | 10 -> "e"
  | 11 -> "E"
  | 12 -> "o"
  | 13 -> "O"
  | 14 -> "M"
  | 15 -> "z"
  | 16 -> "H"
  | 17 -> "k"
  | 18 -> "K"
  | 19 -> "g"
  | 20 -> "G"
  | 21 -> "f"
  | 22 -> "c"
  | 23 -> "C"
  | 24 -> "j"
  | 25 -> "J"
  | 26 -> "F"
  | 27 -> "t"
  | 28 -> "T"
  | 29 -> "d"
  | 30 -> "D"
  | 31 -> "N"
  | 32 -> "w"
  | 33 -> "W"
  | 34 -> "x"
  | 35 -> "X"
  | 36 -> "n"
  | 37 -> "p"
  | 38 -> "P"
  | 39 -> "b"
  | 40 -> "B"
  | 41 -> "m"
  | 42 -> "y"
  | 43 -> "r"
  | 44 -> "l" (* Vedic l not accommodated *)
  | 45 -> "v"
  | 46 -> "S"
  | 47 -> "R"
  | 48 -> "s"
  | 49 -> "h"
  | 50 -> "_" (* hiatus *)
  | -1 -> "Z" (* avagraha *)
  | -2 -> "[-]" (* amuissement - lopa of current aa- or preceding a- or aa- *)
  | -3 -> "A|a" (* sandhi of aa and a *a *)
  | -4 -> "A|i" (* sandhi of aa and i *e *)
  | -5 -> "A|u" (* sandhi of aa and u *u *)
  | -6 -> "A|r" (* sandhi of aa and .r *r *)
  | -7 -> "aa|I" (* sandhi of aa and I *I *)
  | -8 -> "aa|U" (* sandhi of aa and U *U *)
  | -9 -> "aa|A" (* sandhi of aa and A *A *)
  | 123 -> "aa|C" (* sandhi of aa and C *C for duplication *)
  | 100 -> "+" (* explicit compound with no sandhi - experimental *)
  | n -> if n<0 || n>59 then failwith mess
            where mess = "Canon: Illegal char " ^ string_of_int n
         else "#" ^ Char.escaped (Char.chr (n-2)) (* homo index 1 to 9 *)
  ]
;
value decode_WX word =
  List.fold_right (fun c s -> (canon_WX c) ^ s) word ""
;
(* Sanskrit Library SLP1 decoding *)
value canon_SL = fun
  [ 0 -> "-"
  | 100 -> "+"
  | 1  -> "a"
  | 2  -> "A"
  | 3  -> "i"
  | 4  -> "I"
  | 5  -> "u"
  | 6  -> "U"
  | 7  -> "f"
  | 8  -> "F"
  | 9  -> "x"
  | 10 -> "e"
  | 11 -> "E"
  | 12 -> "o"
  | 13 -> "O"
  | 14 -> "M"
  | 15 -> "~"
  | 16 -> "H"
  | 17 -> "k"
  | 18 -> "K"
  | 19 -> "g"
  | 20 -> "G"
  | 21 -> "N"
  | 22 -> "c"
  | 23 -> "C"
  | 24 -> "j"
  | 25 -> "J"
  | 26 -> "Y"
  | 27 -> "w"
  | 28 -> "W"
  | 29 -> "q"
  | 30 -> "Q"
  | 31 -> "R"
  | 32 -> "t"
  | 33 -> "T"
  | 34 -> "d"
  | 35 -> "D"
  | 36 -> "n"
  | 37 -> "p"
  | 38 -> "P"
  | 39 -> "b"
  | 40 -> "B"
  | 41 -> "m"
  | 42 -> "y"
  | 43 -> "r"
  | 44 -> "l" (* Vedic l not accommodated *)
  | 45 -> "v"
  | 46 -> "S"
  | 47 -> "z"
  | 48 -> "s"
  | 49 -> "h"
  | 50 -> "_" (* hiatus *)
  | -1 -> "Z" (* avagraha *)
  | n -> if n<0 || n>59 then failwith mess
            where mess = "Canon: Illegal char " ^ string_of_int n
         else "#" ^ Char.escaped (Char.chr (n-2)) (* homo index 1 to 9 *)
  ]
;
value decode_SL word =
  List.fold_right (fun c s -> (canon_SL c) ^ s) word ""
;
(* Kyoto-Harvard decoding *)
value canon_KH = fun
  [ 0 -> "-"
  | 100 -> "+"
  | 1  -> "a"
  | 2  -> "A"
  | 3  -> "i"
  | 4  -> "I"
  | 5  -> "u"
  | 6  -> "U"
  | 7  -> "R"
  | 8  -> "RR"
  | 9  -> "L"
  | 10 -> "e"
  | 11 -> "ai"
  | 12 -> "o"
  | 13 -> "au"
  | 14 -> "M"
  | 15 -> "M" (* candrabindu absent *)
  | 16 -> "H"
  | 17 -> "k"
  | 18 -> "kh"
  | 19 -> "g"
  | 20 -> "gh"
  | 21 -> "G"
  | 22 -> "c"
  | 23 -> "ch"
  | 24 -> "j"
  | 25 -> "jh"
  | 26 -> "J"
  | 27 -> "T"
  | 28 -> "Th"
  | 29 -> "D"
  | 30 -> "Dh"
  | 31 -> "N"
  | 32 -> "t"
  | 33 -> "th"
  | 34 -> "d"
  | 35 -> "dh"
  | 36 -> "n"
  | 37 -> "p"
  | 38 -> "ph"
  | 39 -> "b"
  | 40 -> "bh"
  | 41 -> "m"
  | 42 -> "y"
  | 43 -> "r"
  | 44 -> "l" (* Vedic l not accommodated *)
  | 45 -> "v"
  | 46 -> "z"
  | 47 -> "S"
  | 48 -> "s"
  | 49 -> "h"
  | 50 -> "_" (* hiatus *)
  | -1 -> "'" (* avagraha *)
  | n -> if n<0 || n>59 then failwith mess
            where mess = "Canon: Illegal char " ^ string_of_int n
         else "#" ^ Char.escaped (Char.chr (n-2)) (* homo index 1 to 9 *)
  ]
;
value decode_KH word =
  List.fold_right (fun c s -> (canon_KH c) ^ s) word ""
;
value switch_decode = fun (* normalizes anusvaara in its input *)
  [ "VH" -> decode 
  | "WX" -> decode_WX
  | "KH" -> decode_KH
  | "SL" -> decode_SL
  | _ -> failwith "Unexpected transliteration scheme"
  ] 
;
(* Decoding without double quotes *)
value canon2 = fun
  [ 0 -> "-"
  | 100 -> "+"
  | 1  -> "a"
  | 2  -> "A"
  | 3  -> "i"
  | 4  -> "I"
  | 5  -> "u"
  | 6  -> "U"
  | 7  -> ".r"
  | 8  -> ".R"
  | 9  -> ".l"
  | 10 -> "e"
  | 11 -> "E"
  | 12 -> "o"
  | 13 -> "O"
  | 14 -> ".m"
  | 15 -> "M" 
  | 16 -> ".h"
  | 17 -> "k"
  | 18 -> "K"
  | 19 -> "g"
  | 20 -> "G"
  | 21 -> "N"
  | 22 -> "c"
  | 23 -> "C"
  | 24 -> "j"
  | 25 -> "J"
  | 26 -> "~n"
  | 27 -> ".t"
  | 28 -> ".T"
  | 29 -> ".d"
  | 30 -> ".D"
  | 31 -> ".n"
  | 32 -> "t"
  | 33 -> "T"
  | 34 -> "d"
  | 35 -> "D"
  | 36 -> "n"
  | 37 -> "p"
  | 38 -> "P"
  | 39 -> "b"
  | 40 -> "B"
  | 41 -> "m"
  | 42 -> "y"
  | 43 -> "r"
  | 44 -> "l"
  | 45 -> "v"
  | 46 -> "z"
  | 47 -> ".s"
  | 48 -> "s"
  | 49 -> "h"
  | 50 -> "_" (* hiatus *)
  | -1 -> "'"
  | -2 -> "[-]" 
  | -3 -> "aa|a" (* sandhi of A and a *a *)
  | -4 -> "aa|i" (* sandhi of A and i *i *) 
  | -5 -> "aa|u" (* sandhi of A and u *u *)
  | -6 -> "aa|r" (* sandhi of A and .r *r *)
  | -7 -> "aa|I" (* sandhi of aa and I *I *)
  | -8 -> "aa|U" (* sandhi of aa and U *U *)
  | -9 -> "aa|A" (* sandhi of aa and A *A *)
  | 123 -> "aa|C" (* sandhi of aa and C *C *)
  | n -> if n<0 || n>59 then failwith ("canon2: " ^ string_of_int n)
         else ("#" ^ Char.escaped (Char.chr (n-2)))
  ]
;
(* hiatus-conscious catenation [b=True] iff [s] starts with vowel *)
value catenate2 c (s,b) = 
  let b'= c>0 && c<14  (* Phonetics.vowel c *) in
  let protected = if b && b' then "_" ^ s else s in
  (canon2 c ^ protected , b')
;
(* decode2 : word -> string (debug for [Morpho_xml] *)
value decode2 word = 
  try let (s,_) = List.fold_right catenate2 word ("",False) in s  
  with [ Failure _ -> failwith ("decode2: " ^ robust_decode (Word.mirror word)) ]
;
value canon_upper = fun
  [ 101 -> "A"
  | 102 -> "AA"
  | 103 -> "I"
  | 104 -> "II"
  | 105 -> "U"
  | 106 -> "UU"
  | 107 -> ".R"
  | 110 -> "E"
  | 111 -> "Ai"
  | 112 -> "O"
  | 113 -> "Au"
  | 117 -> "K"
  | 118 -> "Kh"
  | 119 -> "G"
  | 120 -> "Gh"
  | 122 -> "C"
  | 123 -> "Ch"
  | 124 -> "J"
  | 125 -> "Jh"
  | 127 -> ".T"
  | 128 -> ".Th"
  | 129 -> ".D"
  | 130 -> ".Dh"
  | 132 -> "T"
  | 133 -> "Th"
  | 134 -> "D"
  | 135 -> "Dh"
  | 136 -> "N"
  | 137 -> "P"
  | 138 -> "Ph"
  | 139 -> "B"
  | 140 -> "Bh"
  | 141 -> "M"
  | 142 -> "Y"
  | 143 -> "R"
  | 144 -> "L"
  | 145 -> "V"
  | 146 -> "Z"
  | 147 -> ".S"
  | 148 -> "S"
  | 149 -> "H"
  | n -> failwith ("Illegal upper case code : " ^ string_of_int n)
  ]
;
(* [decode_ref : word -> string] *)
value decode_ref word = 
  let canon c = if c>100 then canon_upper c else canon c in
  let canon_catenate c (s,b) = 
      let b'= c>0 && c<14  (* Phonetics.vowel c *) in
      let protected = if b && b' then "_" ^ s else s in
      (canon c ^ protected , b') in
  let (s,_) = List.fold_right canon_catenate word ("",False) in s 
;
value canon_html = fun
  [ 0  -> "-"
  | 100 -> "+"
  | 1  -> "a"
  | 2  -> "aa"
  | 3  -> "i"
  | 4  -> "ii"
  | 5  -> "u"
  | 6  -> "uu"
  | 7  -> ".r"
  | 8  -> ".rr"
  | 9  -> ".l"
  | 10 -> "e"
  | 11 -> "ai"
  | 12 -> "o"
  | 13 -> "au"
  | 14 -> ".m"
  | 15 -> "~"
  | 16 -> ".h"
  | 17 -> "k"
  | 18 -> "kh"
  | 19 -> "g"
  | 20 -> "gh"
  | 21 -> "f"
  | 22 -> "c"
  | 23 -> "ch"
  | 24 -> "j"
  | 25 -> "jh"
  | 26 -> "~n"
  | 27 -> ".t"
  | 28 -> ".th"
  | 29 -> ".d"
  | 30 -> ".dh"
  | 31 -> ".n"
  | 32 -> "t"
  | 33 -> "th"
  | 34 -> "d"
  | 35 -> "dh"
  | 36 -> "n"
  | 37 -> "p"
  | 38 -> "ph"
  | 39 -> "b"
  | 40 -> "bh"
  | 41 -> "m"
  | 42 -> "y"
  | 43 -> "r"
  | 44 -> "l"
  | 45 -> "v"
  | 46 -> "z"
  | 47 -> ".s"
  | 48 -> "s"
  | 49 -> "h"
  | 50 -> "_" (* hiatus *)
  | n -> if n<0 then 
            failwith ("Illegal letter to canon_html : " ^ string_of_int n)
         else ("#" ^ Char.escaped (Char.chr (n-2)))
  ]
;
value canon_upper_html = fun
  [ 101 -> "Ua"
  | 102 -> "Uaa"
  | 103 -> "Ui"
  | 104 -> "Uii"
  | 105 -> "Uu"
  | 106 -> "Uuu"
  | 107 -> "U.r"
  | 110 -> "Ue"
  | 111 -> "Uai"
  | 112 -> "Uo"
  | 113 -> "Uau"
  | 117 -> "Uk"
  | 118 -> "Ukh"
  | 119 -> "Ug"
  | 120 -> "Ugh"
  | 122 -> "Uc"
  | 123 -> "Uch"
  | 124 -> "Uj"
  | 125 -> "Ujh"
  | 127 -> "U.t"
  | 128 -> "U.th"
  | 129 -> "U.d"
  | 130 -> "U.dh"
  | 132 -> "Ut"
  | 133 -> "Uth"
  | 134 -> "Ud"
  | 135 -> "Udh"
  | 136 -> "Un"
  | 137 -> "Up"
  | 138 -> "Uph"
  | 139 -> "Ub"
  | 140 -> "Ubh"
  | 141 -> "Um"
  | 142 -> "Uy"
  | 143 -> "Ur"
  | 144 -> "Ul"
  | 145 -> "Uv"
  | 146 -> "Uz"
  | 147 -> "U.s"
  | 148 -> "Us"
  | 149 -> "Uh"
  | n -> failwith ("Illegal upper case code : " ^ string_of_int n)
  ]
;
(* Roman with diacritics Unicode - latin extended *)
value canon_uniromcode = fun
  [ 0 -> "-"
  | 100 -> "+"
  | 1  -> "a"
  | 2  -> "&#257;"
  | 3  -> "i"
  | 4  -> "&#299;"
  | 5  -> "u"
  | 6  -> "&#363;"
  | 7  -> "&#7771;"
  | 8  -> "&#7773;"
  | 9  -> "&#7735;"
  | 10 -> "e"
  | 11 -> "ai"
  | 12 -> "o"
  | 13 -> "au"
  | 14 -> "&#7747;" (* anusvaara as m with dot below *)
  | 15 -> "&#7745;" (* candrabindu as m with dot above (?) *)
  | 16 -> "&#7717;"
  | 17 -> "k"
  | 18 -> "kh"
  | 19 -> "g"
  | 20 -> "gh"
  | 21 -> "&#7749;"
  | 22 -> "c"
  | 23 -> "ch"
  | 24 -> "j"
  | 25 -> "jh"
  | 26 -> "&#241;"
  | 27 -> "&#7789;"
  | 28 -> "&#7789;h"
  | 29 -> "&#7693;"
  | 30 -> "&#7693;h"
  | 31 -> "&#7751;"
  | 32 -> "t"
  | 33 -> "th"
  | 34 -> "d"
  | 35 -> "dh"
  | 36 -> "n"
  | 37 -> "p"
  | 38 -> "ph"
  | 39 -> "b"
  | 40 -> "bh"
  | 41 -> "m"
  | 42 -> "y"
  | 43 -> "r"
  | 44 -> "l"
  | 45 -> "v"
  | 46 -> "&#347;"
  | 47 -> "&#7779;"
  | 48 -> "s"
  | 49 -> "h"
  | 50 -> "_"
  | -1 -> "'"
  | -2 -> "[-]" (* amuissement - lopa of current aa- or preceding a- or aa- *)
  | -3 -> "&#257;|a" (* sandhi of aa and (a,aa) *a *)
  | -4 -> "&#257;|i" (* sandhi of aa and (i,ii) *e *)
  | -5 -> "&#257;|u" (* sandhi of aa and (u,uu) *u *)
  | -6 -> "&#257;|r" (* sandhi of aa and .r     *r *)
  | 124 -> failwith "Canon: Unrestored special phoneme j'" 
  | 149 -> failwith "Canon: Unrestored special phoneme h'" 
  | 249 -> failwith "Canon: Unrestored special phoneme h''" 
  | n -> if n<0 then 
            failwith ("Illegal code to canon_unicode : " ^ string_of_int n)
         else ("_" ^ Char.escaped (Char.chr (n-2)))
  ]
;
(* Gives the Unicode representation of the romanisation of word *)
(* [uniromcode : word -> string] *)
value uniromcode word = 
  let catenate c (s,b) = 
      let b'= c>0 && c<14  (* Phonetics.vowel c *) in
      let protected = if b && b' then " " ^ s else s in
      (canon_uniromcode c ^ protected , b') in
  let (s,_) = List.fold_right catenate word ("",False) in s 
;
value halant = "&#x094D;"
(* and avagraha = "&#x093D;"
   and candrabindu = "&#x310;" *)
(* Numerals to come: 1="x0967;" ... 9="x0966F" *)
;
(* represents a stem word in romanization or VH transliteration *)
value stem_to_string html =
  if html then uniromcode (* UTF8 romanization with diacritics *)
          else decode (* VH *)
;

exception Hiatus
;
value indic_unicode_point = fun
  [ 0  | 100 -> (* - + *)  "70"
  | 1  -> (* a *)  "05"
  | 2  -> (* aa *) "06"
  | 3  -> (* i *)  "07"
  | 4  -> (* ii *) "08"
  | 5  -> (* u *)  "09"
  | 6  -> (* uu *) "0A"
  | 7  -> (* .r *) "0B"
  | 8  -> (* .rr *)"60"
  | 9  -> (* .l *) "0C"
  | 10 -> (* e *)  "0F"
  | 11 -> (* ai *) "10"
  | 12 -> (* o *)  "13"
  | 13 -> (* au *) "14"
  | 14 -> (* .m *) "02"
  | 15 -> (* ~~ *) "01"
  | 16 -> (* .h *) "03" 
  | 17 -> (* k *)  "15"
  | 18 -> (* kh *) "16"
  | 19 -> (* g *)  "17"
  | 20 -> (* gh *) "18"
  | 21 -> (* 'n *) "19"
  | 22 -> (* c *)  "1A"
  | 23 -> (* ch *) "1B"
  | 24 -> (* j *)  "1C"
  | 25 -> (* jh *) "1D"
  | 26 -> (* ~n *) "1E"
  | 27 -> (* .t *) "1F"
  | 28 -> (* .th *)"20"
  | 29 -> (* .d *) "21"
  | 30 -> (* .dh *)"22"
  | 31 -> (* .n *) "23"
  | 32 -> (* t *)  "24"
  | 33 -> (* th *) "25"
  | 34 -> (* d *)  "26"
  | 35 -> (* dh *) "27"
  | 36 -> (* n *)  "28"
  | 37 -> (* p *)  "2A"
  | 38 -> (* ph *) "2B"
  | 39 -> (* b *)  "2C"
  | 40 -> (* bh *) "2D"
  | 41 -> (* m *)  "2E"
  | 42 -> (* y *)  "2F"
  | 43 -> (* r *)  "30"
  | 44 -> (* l *)  "32"
  | 45 -> (* v *)  "35"
  | 46 -> (* z *)  "36"
  | 47 -> (* .s *) "37"
  | 48 -> (* s *)  "38"
  | 49 -> (* h *)  "39"
  | 50 -> (* underscore *)  raise Hiatus
  | -1 -> (* avagraha *) "3D"
  | -2 -> "" (* amuissement *)
  | -3 -> "06" (* "aa|a" sandhi of aa and (a,aa) *)
  | -4 -> "0F" (* "aa|i" sandhi of aa and (i,ii) *)
  | -5 -> "13" (* "aa|u" sandhi of aa and (u,uu) *)
  | -6 -> "06" (* sandhi of aa and .r *)
  | 51 (* 1 *) -> "67" (* homo 1 *)
  | 52 (* 2 *) -> "68" 
  | 53 (* 3 *) -> "69" 
  | 54 (* 4 *) -> "6A" 
  | 55 (* 5 *) -> "6B" 
  | 56 (* 6 *) -> "6C" 
  | 57 (* 7 *) -> "6D" 
  | 58 (* 8 *) -> "6E" 
  | 59 (* 9 *) -> "6F" 
  | c -> if c<0 || c>59 
            then failwith ("Illegal code to dev_unicode: " ^ string_of_int c)
         else "" 
  ]
and matra_indic_unicode_point = fun
  [ 100   (* + *) (* necessary for word form ending in consonant *)
  | 0  -> (* - *)  "70" (* id for iics *)
  | 1  -> (* a *)  "" (* default *)
  | 2  -> (* aa *) "3E"
  | 3  -> (* i *)  "3F"
  | 4  -> (* ii *) "40"
  | 5  -> (* u *)  "41"
  | 6  -> (* uu *) "42"
  | 7  -> (* .r *) "43"
  | 8  -> (* .rr *)"44"
  | 9  -> (* .l *) "62"
  | 10 -> (* e *)  "47"
  | 11 -> (* ai *) "48"
  | 12 -> (* o *)  "4B"
  | 13 -> (* au *) "4C"
  | 15 -> (* ~~ *) "01"
  | c -> failwith ("Illegal code to matra_unicode : " ^ string_of_int c)
  ]
;
(* om 50 udatta 51 anudatta 52 grave 53 acute 54 avagraha 3D .ll 61 
   danda 64 ddanda 65 0 66 1 67 2 68 3 69 4 6A 5 6B 6 6C 7 6D 8 6E 9 6F deg 70 *)
value inject_point s = "&#x09" ^ s ^ ";"
;
value deva_unicode c = 
  let s = indic_unicode_point c in inject_point s 
and matra_unicode c =
  if c=1 then "" (* default *)
  else let s = matra_indic_unicode_point c in inject_point s 
;
(* Gives the Unicode representation of devanagari form of word;          *)
(* ligature construction is left to the font manager handling of halant. *)
(* [unidevcode : word -> string] *)
value unidevcode word = 
  let ligature (s,b) c = (* b memorizes whether last char is consonant *)
     try let code = deva_unicode c in
         if c>16 && c<50 (* Phonetics.consonant c *) then 
            if b (* add glyph *) then (s ^ halant ^ code,True)
            else (s ^ code,True) 
         else if b then 
              if c=0 (* - *) || c>50 (* homo *) then (s ^ halant ^ code,False)
              else (* add matra *) let m = matra_unicode c in (s ^ m,False)
         else (s ^ code,False) 
     with (* hiatus represented by space in devanagarii output *)
          [ Hiatus -> (s ^ " ",False) ] in 
  let (s,b) =  List.fold_left ligature ("",False) word in
  if b then s ^ halant (* virama *) else s
;

(*i end; i*)

