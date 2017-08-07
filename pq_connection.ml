(* a layer over tcp/ip; send a msg (a string) by first sending length *)

open Pq_pervasive

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


let p = mk_profiler ()
let unix_read_p = p

(* repeatedly read until all read, or exception; buf is filled from 0; assume len < |buf| *)
let unix_read ~conn ~buf ~len = 
  let rec f off = 
    assert (p.mark' P.ab);
    assert_(off <=len);
    if (off=len) then () else
      begin
        assert (p.mark' P.bc);
        (* while (not (can_recv conn)) do Thread.delay 0.000001 done;  (* ! *) *)
        assert (p.mark' P.cd);
        Unix.read conn buf off (len-off) |> fun nread ->
        assert (p.mark' P.de);
        assert_(nread>0);  (* this needs to be checked always, otherwise possibility of loop *)
        f (off+nread)
      end
  in
  f 0







type conn = Unix.file_descr


(* accept connections for this quad only *)
let listen_accept ~quad = 
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
  (match !accept_server with None -> () | Some s -> pq_close_noerr s; accept_server:=None);
  (* close conn if not connected to the right dest *)
  (match !conn with 
   | None -> ()
   | Some c -> 
     if Unix.getpeername c <> Unix.ADDR_INET(ip2,port2) then 
       (pq_close_noerr c; conn:=None)
     else 
       ());
  match (err,!conn) with 
  | (Some e,None) -> raise e
  | (Some e,Some c) -> (pq_close_noerr c; conn:=None; raise e)
  | (None,None) -> assert false
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
  | (Some e,None) -> raise (Pq_exc __LOC__)
  | (Some e,Some c) -> (pq_close_noerr c; client:=None; raise (Pq_exc __LOC__))
  | (None,None) -> assert false (* can't happen *)
  | (None,Some c) -> c


(* send length as 4 bytes, then the string itself; NOTE it is quite
   important to try to call write with a buffer which includes everything
   to do with the message *)
let send_string ~conn ~string_ : unit =
  let err = 
    try
      String.length string_ |> fun len ->
      let buf = Bytes.create (4+len) in
      i2bs ~buf ~off:0 len;
      Bytes.blit_string string_ 0 buf 4 len;
      (* now write the buffer *)
      Unix.write conn buf 0 (4+len) |> fun nwritten ->
      assert_(nwritten=4+len);
      None
    with e -> Some e
  in
  log_if_some __LOC__ err;
  match err with 
  | None -> () 
  | Some e -> raise (Pq_exc __LOC__)

let send ~conn ~strings : unit =
  Marshal.to_string strings [] |> fun string_ ->
  send_string ~conn ~string_


let p = mk_profiler ()
let read_length_p = p

let read_length ~conn : int = (
  assert (p.mark' P.ef);
  Bytes.create 4 |> fun buf ->
  assert (p.mark' P.fg);
  unix_read ~conn ~buf ~len:4 |> fun () ->  (* ! *)
  assert (p.mark' P.gh);
  bs2i ~buf ~off:0 |> fun i ->
  assert (p.mark' P.hi);
  i)

let p = mk_profiler ()
let recv_string_p = p

let recv_string ~conn : string = 
  let s = ref None in
  let err = 
    try
      (* read length *)
      assert (p.mark' P.ab);
      read_length ~conn |> fun len ->  (* ! slow a bit *)
      assert (p.mark' P.bc);
      (* now read the string *)
      Bytes.create len |> fun buf ->          
      assert (p.mark' P.cd);
      unix_read ~conn ~buf ~len |> fun () ->  (* !really slow *)
      assert (p.mark' P.de);      
      s:=Some(Bytes.unsafe_to_string buf);
      None
    with e -> Some e
  in
  log_if_some __LOC__ err;
  match err,!s with
  | None,None -> assert false 
  | Some e, Some s -> assert false
  | None,Some s -> s
  | Some e,None -> raise (Pq_exc __LOC__)

let p = mk_profiler ()
let recv_p = p

let recv ~conn : string list = 
  try
    assert (p.mark' P.ab);
    recv_string ~conn |> fun s ->
    assert (p.mark' P.bc);
    Marshal.from_string s 0 |> fun (ss:string list) ->
    assert (p.mark' P.cd);
    ss
  with e -> (
      log_exn __LOC__ e;
      raise (Pq_exc __LOC__))

