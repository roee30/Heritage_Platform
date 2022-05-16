(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*              Gérard Huet & Amba Kulkarni & Sriram Krishnan             *)
(*                                                                        *)
(* ©2021 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(*i module Make_trans_freq = struct  i*)

(* Prepares the dump of the transition-freq table
   from the text files in Resources/DATA  *)
   
(* The frequency tables are generated using a parallel corpus obtained by 
   mapping DCS Sentences and the analysis of the Heritage Reader. 
   These are stored in txt files in Resources/DATA, and retrieved here to 
   generate rem files for the same *)
   
(* This list of files is converted to decorated tries here:
   a. [comp_trans_freq_file.txt] -> sandhi within compounds and their frequencies
   b. [pada_trans_freq_file.txt] -> sandhi between words and their frequencies
*)

(* Transition and their frequencies are in this format:
    <string, string, integer> *)

(* To get the list of transitions in the format u,v,f, 
   where u|v -> w is the transition and f is the frequency *)
value rec read_file chin acc fmt =
  try let line = input_line chin in
      let (a,b,c) = Scanf.sscanf line fmt (fun x y z -> (x,y,z)) in
      let acc = List.append acc [(a,b,c)] in
      read_file chin acc fmt
  with [
    End_of_file -> do {
      close_in chin
    ; acc
    }
  ]
;
(* To dump the transition list obtained in .rem format *)
value make_trans txt_file fmt rem_file =
  let chin = open_in txt_file in
  let output = read_file chin [] fmt in
  Gen.dump output rem_file
;
value main = 
    make_trans Sys.argv.(1) "%s@\t%s@\t%d" Sys.argv.(2)
;
