(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                              Gérard Huet                               *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Date utilities *)

(*i module Date = struct i*)

value date_iso = Version.version_date (* "YYYY-MM-DD" *)
;
value version_id = Version.version
;
value version = "Version " ^ version_id ^ " [" ^ date_iso ^ "]"
;
value dico_date = (* for Sanskrit-French book form *)
  let year = String.sub date_iso 0 4
  and month = String.sub date_iso 5 2 
  and day = String.sub date_iso 8 2 in
  (match int_of_string day with [ 1 -> "1er" | n -> string_of_int n ]) ^
  (match month with [ "01" -> " Janvier " 
                    | "02" -> " F\\'evrier "
                    | "03" -> " Mars "
                    | "04" -> " Avril "
                    | "05" -> " Mai "
                    | "06" -> " Juin "
                    | "07" -> " Juillet "
                    | "08" -> " Ao\\^ut "
                    | "09" -> " Septembre "
                    | "10" -> " Octobre "
                    | "11" -> " Novembre "
                    | "12" -> " D\\'ecembre "
                    | _ -> failwith "Invalid month code in date"
                    ]) ^ year
;
(*i end; i*)
