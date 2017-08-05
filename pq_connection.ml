(* a layer over tcp/ip; send a msg (a string) by first sending length *)

open Pq_pervasive

type conn = Unix.file_descr

exception Exception of string

let log_if_some loc = function
  | None -> ()
  | Some e -> print_endline @@ loc^": "^(Printexc.to_string e)


(* accept connections for this quad only *)
let listen ~quad = 
  let (ip1,port1,ip2,port2) = quad in
  let accept_server = ref None in
  let conn = ref None in
  let err = 
    try 
      Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 |> fun srvr ->
      accept_server := Some srvr;
	    (* hack to speed up recovery *)
	    Unix.setsockopt srvr Unix.SO_REUSEADDR true;
      let addr = Unix.ADDR_INET (ip1,port1) in
	    Unix.bind srvr addr;
	    Unix.listen srvr 5;
      Unix.accept srvr |> fun (c,_) ->
      conn := Some c;
      None
    with e -> Some e
  in
  log_if_some __LOC__ err;  
  (match !accept_server with | None -> () | Some s -> close_fd_noerr s);
  (* close conn if not connected to the right dest *)
  (match !conn with 
   | None -> ()
   | Some c -> 
	   if Unix.getpeername c <> Unix.ADDR_INET(ip2,port2) then 
	     (close_fd_noerr c; conn:=None)
	   else 
	     ());
  match (err,!conn) with 
  | (Some e,None) -> raise e
  | (Some e,Some c) -> (close_fd_noerr c; raise e)
  | (None,None) -> raise (Exception __LOC__)
  | (None,Some c) -> c


let connect ~quad = 
  let client = ref None in
  let err = 
    try 
      let (ip1,port1,ip2,port2) = quad in
      let src = Unix.ADDR_INET (ip1,port1) in
      let dst = Unix.ADDR_INET (ip2,port2) in
      Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 |> fun c ->
      client := Some c;
	    (* hack to speed up recovery *)
	    Unix.setsockopt c Unix.SO_REUSEADDR true;
	    Unix.bind c src;
	    Unix.connect c dst;
      None
    with e -> Some e
  in
  log_if_some __LOC__ err;
  match (err,!client) with
  | (Some e,None) -> raise (Exception __LOC__)
  | (Some e,Some c) -> (close_fd_noerr c; raise (Exception __LOC__))
  | (None,None) -> assert false (* can't happen *)
  | (None,Some c) -> c

let write_length ~conn ~len = (
  i2bs len |> fun bs ->  
  Unix.write conn bs 0 4 |> fun n ->
  assert(n=4);
  ())

(* send length as 4 bytes, then the string itself *)
let send_string ~conn ~string_ : unit =
  let err = 
    try
      (* write length *)
      String.length string_ |> fun len ->
      write_length ~conn ~len |> fun () ->
      (* now write the string itself *)
      Unix.write conn (Bytes.of_string string_) 0 len |> fun nwritten ->
      assert(nwritten=len);
      None
    with e -> Some e
  in
  log_if_some __LOC__ err;
  match err with 
  | None -> () 
  | Some e -> raise (Exception __LOC__)

let send ~conn ~strings : unit =
  Marshal.to_string strings [] |> fun string_ ->
  send_string ~conn ~string_


let can_recv ~conn =
  try
    let (r,_,_) = Unix.select [conn] [] [] 0.0 in
    not (r=[])
  with _ -> raise (Exception __LOC__)


let read_length ~conn : int = (
  Bytes.create 4 |> fun buf ->
  unix_read ~conn ~buf ~len:4 |> fun () ->
  bs2i buf)

let recv_string ~conn : string = 
  let s = ref None in
  let err = 
    try
      (* read length *)
      read_length ~conn |> fun len ->
      (* now read the string *)
      Bytes.create len |> fun buf ->          
      unix_read ~conn ~buf ~len |> fun () ->
      s:=Some(Bytes.unsafe_to_string buf);
      None
    with e -> Some e
  in
  log_if_some __LOC__ err;
  match err,!s with
  | None,None -> assert false 
  | Some e, Some s -> assert false
  | None,Some s -> s
  | Some e,None -> raise (Exception __LOC__)

let recv ~conn : string list = 
  try
    recv_string ~conn |> fun s ->
    Marshal.from_string s 0 |> fun (ss:string list) ->
    ss
  with _ -> raise (Exception __LOC__)


let close ~conn = 
  try Unix.close conn 
  with e -> 
    print_endline @@ __LOC__^": close: "^(Printexc.to_string e);
    raise (Exception __LOC__)


let close_no_err ~conn = close_fd_noerr conn
