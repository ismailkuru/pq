open Net_pervasive

let connect ~quad = 
  let client = ref None in
  let err = 
    try 
      Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 |> fun c ->
      client := Some c;
      (* hack to speed up recovery *)
      Unix.setsockopt c Unix.SO_REUSEADDR true;
      Unix.bind c quad.local;
      Unix.connect c quad.remote;
      None
    with e -> Some e
  in
  log_if_some __LOC__ err;
  match (err,!client) with
  | (Some e,None) -> raise (Pq_exc __LOC__)
  | (Some e,Some c) -> (pq_close_noerr c; client:=None; raise (Pq_exc __LOC__))
  | (None,None) -> assert false (* can't happen *)
  | (None,Some c) -> c
