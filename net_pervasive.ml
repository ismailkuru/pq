include Tjr_profile

let print_string s = 
  print_string s; 
  flush stdout

let print_endline s = print_string @@ s^"\n"

let debug s = () (* print_string s *)

(* assert that is always checked *)
let assert_ b = if b then () else assert false

type conn = Unix.file_descr
type ip = Unix.inet_addr
type port = int
type ipp = Unix.sockaddr (*  expect ADDR_INET ip * port *)
type quad = { local:ipp; remote: ipp }

let is_Some e = match e with Some e -> true | _ -> false
let dest_Some e = match e with Some e -> e | _ -> failwith "dest_Some"
let maybe_raise e = match e with None -> () | Some e -> raise e



(* int <-> byte_x4 conversion *)

let i2bs ~buf ~off i = 
  assert_ (i>=0);
  let i = ref i in
  for j = 0 to 3 do
    Bytes.set buf (off+j) ((!i) mod 256 |> Char.chr);
    i:=!i / 256
  done;
  ()

let bs2i ~buf ~off = 
  assert_ (Bytes.length buf >= 4+off);
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



let log_exn loc e = print_endline @@ loc^": "^(Printexc.to_string e)
let log_if_some loc = function
  | None -> ()
  | Some e -> log_exn loc e


let can_recv ~conn =
  try
    let (r,_,_) = Unix.select [conn] [] [] 0.0 in
    not (r=[])
  with e -> (
      log_exn __LOC__ e;
      raise (Pq_exc __LOC__))



