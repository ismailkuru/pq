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


let close_fd_noerr fd = 
  try Unix.close fd 
  with e -> 
    print_endline @@ __LOC__^": close_fd_noerr: "^(Printexc.to_string e)


(* int <-> byte_x4 conversion *)

let i2bs i = 
  assert (i>=0);
  Bytes.create 4 |> fun buf ->
  let i = ref i in
  for j = 0 to 3 do
    Bytes.set buf j ((!i) mod 256 |> Char.chr);
    i:=!i / 256
  done;
  buf

let bs2i buf = 
  assert(Bytes.length buf = 4);
  let i = ref 0 in
  for j = 3 downto 0 do
    Bytes.get buf j |> Char.code |> fun x ->
    i:=256*(!i) + x;
  done;
  (!i)


(* repeatedly read until all read, or exception *)
let unix_read ~conn ~buf ~len = 
  let rec f off = 
    assert(off <=len);
    if (off=len) then () else
      Unix.read conn buf off (len-off) |> fun nread ->
      assert(nread>0);
      f (off+nread)
  in
  f 0


