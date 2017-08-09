(* single-shot listen, with specific local/remote address *)

open Net_pervasive

(* accept connections for this quad only *)
let listen_accept ~quad = 
  let accept_server = ref None in
  let conn = ref None in
  let err = 
    try 
      Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 |> fun srvr ->
      accept_server := Some srvr;
      (* hack to speed up recovery *)
      Unix.setsockopt srvr Unix.SO_REUSEADDR true;
      let addr = quad.local in
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
     if Unix.getpeername c <> quad.remote then 
       (pq_close_noerr c; conn:=None)
     else 
       ());
  match (err,!conn) with 
  | (Some e,None) -> raise e
  | (Some e,Some c) -> (pq_close_noerr c; conn:=None; raise e)
  | (None,None) -> assert false
  | (None,Some c) -> c
