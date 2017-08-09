(* a layer over tcp/ip; send a msg (a string) by first sending length *)

open Net_pervasive


(* receive ---------------------------------------------------------- *)

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
        Unix.read conn buf off (len-off) |> fun nread ->
        assert (p.mark' P.de);
        assert_(nread>0);  (* this needs to be checked always, otherwise possibility of loop *)
        f (off+nread)
      end
  in
  f 0


let p = mk_profiler ()
let read_length_p = p

(* internal *)
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

let recv_strings ~conn : string list = 
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



(* send ------------------------------------------------------------- *)


(* send length as 4 bytes, then the string itself; NOTE for
   performance, it is quite important to try to call write with a
   buffer which includes everything to do with the message *)
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

let send_strings ~conn ~strings : unit =
  Marshal.to_string strings [] |> fun string_ ->
  send_string ~conn ~string_



