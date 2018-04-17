(**************************************************************************)
(*                                                                        *)
(*                     The Sanskrit Heritage Platform                     *)
(*                                                                        *)
(*                       Gérard Huet & Pawan Goyal                        *)
(*                                                                        *)
(* ©2018 Institut National de Recherche en Informatique et en Automatique *)
(**************************************************************************)

(* Tests consistency of data version of [Heritage_Resources]
   with program version of [Heritage_Platform] *)

value check_data_version () = 
  let resources_version_file = Paths.skt_resources_dir ^ "DATA/version.rem" in
  let (data_format_version, data_version) = 
      (Gen.gobble resources_version_file : (int * string)) in
   if Control.data_format_version > data_format_version then do
      { print_string "Your linguistic data are stale\n"
      ; print_string "Your must install a recent version of Heritage_Resources\n"
      ; exit 1
      } else
   if Control.data_format_version < data_format_version then do
     { print_string "Your Heritage_Platform installation is too old\n"
     ; print_string "Your must re-install a more recent version\n"
     ; print_string "consistent with data format "
     ; print_int data_format_version
     ; print_string "\n"
     ; exit 1
     } else
   if data_version = Version.version then () else do
     { print_string "Warning: this platform release "
     ; print_string ("assumes version " ^ Version.version)
     ; print_string " of Heritage_Resources\n"
     ; print_string "while the currently installed Heritage_Resources at "
     ; print_string resources_version_file
     ; print_string (" has version " ^ data_version)
     ; print_string "\n You should consider updating to recent versions\n"
     }
;
try check_data_version () with
[ Sys_error m -> failwith ("Wrong structure of Heritage_Resources " ^ m) ]
;
