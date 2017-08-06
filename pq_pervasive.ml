include Tjr_profile

let print_string s = 
  print_string s; 
  flush stdout

let print_endline s = print_string @@ s^"\n"

let debug s = () (* print_string s *)


type conn = Unix.file_descr
type ip = Unix.inet_addr
type port = int
type quad = ip*port*ip*port

let is_Some e = match e with Some e -> true | _ -> false
let dest_Some e = match e with Some e -> e | _ -> failwith "dest_Some"
let maybe_raise e = match e with None -> () | Some e -> raise e



(* int <-> byte_x4 conversion *)

let i2bs ~buf ~off i = 
  assert (i>=0);
  let i = ref i in
  for j = 0 to 3 do
    Bytes.set buf (off+j) ((!i) mod 256 |> Char.chr);
    i:=!i / 256
  done;
  ()

let bs2i ~buf ~off = 
  assert(Bytes.length buf >= 4+off);
  let i = ref 0 in
  for j = 3 downto 0 do
    Bytes.get buf (off+j) |> Char.code |> fun x ->
    i:=256*(!i) + x;
  done;
  (!i)


exception Pq_exc of string

let pq_close_noerr fd = 
  try Unix.close fd 
  with e -> 
    print_endline @@ __LOC__^": pq_close_noerr: "^(Printexc.to_string e)


let pq_close ~conn = 
  try Unix.close conn 
  with e -> 
    print_endline @@ __LOC__^": pq_close: "^(Printexc.to_string e);
    raise (Pq_exc __LOC__)

