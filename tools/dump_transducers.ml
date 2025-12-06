(* Dump transducers to a portable msgpack format for Python loader.
   Usage:
     dune exec -- ./dump_transducers <output.msgpack>
   Default output: python/heritage/data/transducers.msgpack
   
   Note: msgpack is a binary format that's significantly faster and more compact
   than JSON for serialization/deserialization of complex data structures.
*)
open Printf

open Auto.Auto
open Automata_vector

let out_file = if Array.length Sys.argv > 1 then Sys.argv.(1) else "python/heritage/data/transducers.msgpack"

(* Mutable state for building nodes *)
let next_id = ref 0
let tbl : (auto, int) Hashtbl.t = Hashtbl.create 65536
let node_buf : (int * Msgpack.t) list ref = ref []

(* Convert word (list of ints) to msgpack array *)
let msgpack_word w = 
  `FixArray (List.map (fun x -> `Uint16 x) w)

(* Convert rule to msgpack array [w, u, v] *)
let msgpack_rule (w, u, v) =
  `FixArray [msgpack_word w; msgpack_word u; msgpack_word v]

(* Convert rules list to msgpack array *)
let msgpack_rules rules =
  `FixArray (List.map msgpack_rule rules)

let rec visit_auto a =
  try Hashtbl.find tbl a
  with Not_found ->
    let id = !next_id in
    incr next_id;
    Hashtbl.add tbl a id;
    (* decompose state *)
    let node_msgpack =
      match a with
      | State ((accept, deter, rules)) ->
          (* deter : (Word.letter * auto) list *)
          let arcs_list =
            deter
            |> List.map (fun (ch, targ) ->
                   let tid = visit_auto targ in
                   `FixArray [`Uint16 ch; `Uint16 tid])
          in
          let arcs = `FixArray arcs_list in
          let rules_msgpack = msgpack_rules rules in
          `FixMap [
            (`FixRaw (List.init 6 (String.get "accept")), `Bool accept);
            (`FixRaw (List.init 4 (String.get "arcs")), arcs);
            (`FixRaw (List.init 5 (String.get "rules")), rules_msgpack)
          ]
    in
    node_buf := (id, node_msgpack) :: !node_buf;
    id

let write_transducers transducers =
  (* Visit each named automaton to ensure nodes are assigned ids *)
  let fields = [
    ("nouns", transducers.nouns);
    ("kama", transducers.kama);
    ("pronouns", transducers.pronouns);
    ("roots", transducers.roots);
    ("lopas", transducers.lopas);
    ("parts", transducers.parts);
    ("lopaks", transducers.lopaks);
    ("partvocs", transducers.partvocs);
    ("iics", transducers.iics);
    ("iifcs", transducers.iifcs);
    ("avyayais", transducers.avyayais);
    ("avyayafs", transducers.avyayafs);
    ("vocas", transducers.vocas);
    ("vocaf", transducers.vocaf);
    ("invs", transducers.invs);
    ("piics", transducers.piics);
    ("ifcs", transducers.ifcs);
    ("indecls", transducers.indecls);
    ("indifcs", transducers.indifcs);
    ("inftu", transducers.inftu);
    ("absya", transducers.absya);
    ("abstvaa", transducers.abstvaa);
    ("iivs", transducers.iivs);
    ("peris", transducers.peris);
    ("auxis", transducers.auxis);
    ("auxiinvs", transducers.auxiinvs);
    ("auxiks", transducers.auxiks);
    ("auxiicks", transducers.auxiicks);
    ("preverbs", transducers.preverbs)
  ] in
  let roots = List.map (fun (name, a) -> (name, visit_auto a)) fields in

  (* Build nodes array in id order *)
  let nodes_arr = Array.make !next_id `Nil in
  List.iter (fun (id, mp) -> nodes_arr.(id) <- mp) (List.rev !node_buf);

  (* Build roots map *)
  let roots_map =
    roots
    |> List.map (fun (name, id) -> 
         (`FixRaw (List.init (String.length name) (String.get name)), `Uint16 id))
  in

  (* Create final msgpack structure *)
  let final = `FixMap [
    (`FixRaw (List.init 7 (String.get "version")), `Uint16 1);
    (`FixRaw (List.init 5 (String.get "roots")), `Map16 roots_map);
    (`FixRaw (List.init 5 (String.get "nodes")), `Array32 (Array.to_list nodes_arr))
  ] in

  let oc = open_out_bin out_file in
  (* Serialize msgpack structure using msgpack_to_bytes *)
  let rec msgpack_to_bytes = function
    | `Nil -> "\xc0"
    | `Bool true -> "\xc3"
    | `Bool false -> "\xc2"
    | `PFixnum n when n >= 0 && n <= 127 -> String.make 1 (Char.chr n)
    | `PFixnum n -> failwith (sprintf "PFixnum out of range: %d" n)
    | `NFixnum n when n >= -32 && n <= -1 -> String.make 1 (Char.chr (256 + n))
    | `Uint8 n -> "\xcc" ^ String.make 1 (Char.chr n)
    | `Uint16 n -> 
        let hi = Char.chr ((n lsr 8) land 0xFF) in
        let lo = Char.chr (n land 0xFF) in
        "\xcd" ^ (String.make 1 hi) ^ (String.make 1 lo)
    | `Uint32 n ->
        let b3 = Char.chr (Int64.to_int (Int64.shift_right_logical n 24) land 0xFF) in
        let b2 = Char.chr (Int64.to_int (Int64.shift_right_logical n 16) land 0xFF) in
        let b1 = Char.chr (Int64.to_int (Int64.shift_right_logical n 8) land 0xFF) in
        let b0 = Char.chr (Int64.to_int n land 0xFF) in
        "\xce" ^ (String.make 1 b3) ^ (String.make 1 b2) ^ (String.make 1 b1) ^ (String.make 1 b0)
    | `Int8 n -> "\xd0" ^ String.make 1 (Char.chr (if n < 0 then 256 + n else n))
    | `Int16 n -> 
        let hi = Char.chr ((n lsr 8) land 0xFF) in
        let lo = Char.chr (n land 0xFF) in
        "\xd1" ^ (String.make 1 hi) ^ (String.make 1 lo)
    | `FixArray arr ->
        let len = List.length arr in
        if len <= 15 then
          String.make 1 (Char.chr (0x90 + len)) ^ String.concat "" (List.map msgpack_to_bytes arr)
        else if len <= 0xFFFF then
          let len_hi = Char.chr ((len lsr 8) land 0xFF) in
          let len_lo = Char.chr (len land 0xFF) in
          "\xdc" ^ (String.make 1 len_hi) ^ (String.make 1 len_lo) ^
          String.concat "" (List.map msgpack_to_bytes arr)
        else if len <= 0xFFFFFFFF then
          let len_b3 = Char.chr ((len lsr 24) land 0xFF) in
          let len_b2 = Char.chr ((len lsr 16) land 0xFF) in
          let len_b1 = Char.chr ((len lsr 8) land 0xFF) in
          let len_b0 = Char.chr (len land 0xFF) in
          "\xdd" ^ (String.make 1 len_b3) ^ (String.make 1 len_b2) ^ (String.make 1 len_b1) ^ (String.make 1 len_b0) ^
          String.concat "" (List.map msgpack_to_bytes arr)
        else failwith "Array too large"
    | `Array16 arr ->
        let len = List.length arr in
        if len <= 0xFFFF then
          let len_hi = Char.chr ((len lsr 8) land 0xFF) in
          let len_lo = Char.chr (len land 0xFF) in
          "\xdc" ^ (String.make 1 len_hi) ^ (String.make 1 len_lo) ^
          String.concat "" (List.map msgpack_to_bytes arr)
        else failwith "Array16 length > 0xFFFF"
    | `Array32 arr ->
        let len = List.length arr in
        let len_b3 = Char.chr ((len lsr 24) land 0xFF) in
        let len_b2 = Char.chr ((len lsr 16) land 0xFF) in
        let len_b1 = Char.chr ((len lsr 8) land 0xFF) in
        let len_b0 = Char.chr (len land 0xFF) in
        "\xdd" ^ (String.make 1 len_b3) ^ (String.make 1 len_b2) ^ (String.make 1 len_b1) ^ (String.make 1 len_b0) ^
        String.concat "" (List.map msgpack_to_bytes arr)
    | `FixRaw chars ->
        let bytes = String.of_seq (List.to_seq chars) in
        let len = String.length bytes in
        if len <= 31 then
          String.make 1 (Char.chr (0xa0 + len)) ^ bytes
        else failwith "FixRaw length > 31"
    | `Raw16 chars ->
        let bytes = String.of_seq (List.to_seq chars) in
        let len = String.length bytes in
        if len <= 0xFFFF then
          let len_hi = Char.chr ((len lsr 8) land 0xFF) in
          let len_lo = Char.chr (len land 0xFF) in
          "\xda" ^ (String.make 1 len_hi) ^ (String.make 1 len_lo) ^ bytes
        else failwith "Raw16 length > 0xFFFF"
    | `Raw32 chars ->
        let bytes = String.of_seq (List.to_seq chars) in
        let len = String.length bytes in
        let len_b3 = Char.chr ((len lsr 24) land 0xFF) in
        let len_b2 = Char.chr ((len lsr 16) land 0xFF) in
        let len_b1 = Char.chr ((len lsr 8) land 0xFF) in
        let len_b0 = Char.chr (len land 0xFF) in
        "\xdb" ^ (String.make 1 len_b3) ^ (String.make 1 len_b2) ^ (String.make 1 len_b1) ^ (String.make 1 len_b0) ^ bytes
    | `FixMap pairs ->
        let len = List.length pairs in
        if len <= 15 then
          String.make 1 (Char.chr (0x80 + len)) ^ 
          String.concat "" (List.map (fun (k, v) -> msgpack_to_bytes k ^ msgpack_to_bytes v) pairs)
        else failwith "FixMap length > 15"
    | `Map16 pairs ->
        let len = List.length pairs in
        if len <= 0xFFFF then
          let len_hi = Char.chr ((len lsr 8) land 0xFF) in
          let len_lo = Char.chr (len land 0xFF) in
          "\xde" ^ (String.make 1 len_hi) ^ (String.make 1 len_lo) ^
          String.concat "" (List.map (fun (k, v) -> msgpack_to_bytes k ^ msgpack_to_bytes v) pairs)
        else failwith "Map16 length > 0xFFFF"
    | `Map32 pairs ->
        let len = List.length pairs in
        let len_b3 = Char.chr ((len lsr 24) land 0xFF) in
        let len_b2 = Char.chr ((len lsr 16) land 0xFF) in
        let len_b1 = Char.chr ((len lsr 8) land 0xFF) in
        let len_b0 = Char.chr (len land 0xFF) in
        "\xdf" ^ (String.make 1 len_b3) ^ (String.make 1 len_b2) ^ (String.make 1 len_b1) ^ (String.make 1 len_b0) ^
        String.concat "" (List.map (fun (k, v) -> msgpack_to_bytes k ^ msgpack_to_bytes v) pairs)
    | `Float f ->
        let bits = Int32.bits_of_float f in
        let b3 = Char.chr (Int32.to_int (Int32.shift_right_logical bits 24) land 0xFF) in
        let b2 = Char.chr (Int32.to_int (Int32.shift_right_logical bits 16) land 0xFF) in
        let b1 = Char.chr (Int32.to_int (Int32.shift_right_logical bits 8) land 0xFF) in
        let b0 = Char.chr (Int32.to_int bits land 0xFF) in
        "\xca" ^ (String.make 1 b3) ^ (String.make 1 b2) ^ (String.make 1 b1) ^ (String.make 1 b0)
    | `Double f ->
        let bits = Int64.bits_of_float f in
        let b7 = Char.chr (Int64.to_int (Int64.shift_right_logical bits 56) land 0xFF) in
        let b6 = Char.chr (Int64.to_int (Int64.shift_right_logical bits 48) land 0xFF) in
        let b5 = Char.chr (Int64.to_int (Int64.shift_right_logical bits 40) land 0xFF) in
        let b4 = Char.chr (Int64.to_int (Int64.shift_right_logical bits 32) land 0xFF) in
        let b3 = Char.chr (Int64.to_int (Int64.shift_right_logical bits 24) land 0xFF) in
        let b2 = Char.chr (Int64.to_int (Int64.shift_right_logical bits 16) land 0xFF) in
        let b1 = Char.chr (Int64.to_int (Int64.shift_right_logical bits 8) land 0xFF) in
        let b0 = Char.chr (Int64.to_int bits land 0xFF) in
        "\xcb" ^ (String.make 1 b7) ^ (String.make 1 b6) ^ (String.make 1 b5) ^ (String.make 1 b4) ^
        (String.make 1 b3) ^ (String.make 1 b2) ^ (String.make 1 b1) ^ (String.make 1 b0)
    | `Int32 n -> 
        let b3 = Char.chr (Int32.to_int (Int32.shift_right_logical n 24) land 0xFF) in
        let b2 = Char.chr (Int32.to_int (Int32.shift_right_logical n 16) land 0xFF) in
        let b1 = Char.chr (Int32.to_int (Int32.shift_right_logical n 8) land 0xFF) in
        let b0 = Char.chr (Int32.to_int n land 0xFF) in
        "\xd2" ^ (String.make 1 b3) ^ (String.make 1 b2) ^ (String.make 1 b1) ^ (String.make 1 b0)
    | `Int64 n ->
        let b7 = Char.chr (Int64.to_int (Int64.shift_right_logical n 56) land 0xFF) in
        let b6 = Char.chr (Int64.to_int (Int64.shift_right_logical n 48) land 0xFF) in
        let b5 = Char.chr (Int64.to_int (Int64.shift_right_logical n 40) land 0xFF) in
        let b4 = Char.chr (Int64.to_int (Int64.shift_right_logical n 32) land 0xFF) in
        let b3 = Char.chr (Int64.to_int (Int64.shift_right_logical n 24) land 0xFF) in
        let b2 = Char.chr (Int64.to_int (Int64.shift_right_logical n 16) land 0xFF) in
        let b1 = Char.chr (Int64.to_int (Int64.shift_right_logical n 8) land 0xFF) in
        let b0 = Char.chr (Int64.to_int n land 0xFF) in
        "\xd3" ^ (String.make 1 b7) ^ (String.make 1 b6) ^ (String.make 1 b5) ^ (String.make 1 b4) ^
        (String.make 1 b3) ^ (String.make 1 b2) ^ (String.make 1 b1) ^ (String.make 1 b0)
    | `Uint64 _ -> failwith "Uint64 not implemented"
  in
  let data = msgpack_to_bytes final in
  output_string oc data; close_out oc;
  printf "Wrote %s (nodes=%d, msgpack format)\n" out_file !next_id

let () =
  try
    let trans = (Gen.gobble Data.public_transducers_file : transducers_datatype) in
    (* reset state *)
    next_id := 0; Hashtbl.reset tbl; node_buf := [];
    write_transducers trans
  with e ->
    eprintf "Error: %s\n" (Printexc.to_string e); exit 2
