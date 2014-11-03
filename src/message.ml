(* 

sender error trace:

message.ml/connect, err
sender: initializing
message.ml/connect, err
sender: initializing
message.ml/connect, err
sender: initializing
file.ml/write, exception
sender.ml: unknown exception
Fatal error: exception Sys_error("Connection refused")

*)

let print_string s = (print_string s; flush stdout);;
let debug s = Common.debug ("message.ml/"^s^"\n");;

exception Exception;;

type conn = Unix.file_descr;;
type ip = Unix.inet_addr;;
type port = int;;
type quad = ip*port*ip*port;;

let is_Some e = match e with Some e -> true | _ -> false;;
let dest_Some e = match e with Some e -> e | _ -> failwith "dest_Some";;
let maybe_raise e = match e with | None -> () | Some e -> raise e;;


let close_fd_noerr fd = try Unix.close fd with _ -> ();;

(* accept connections for this quad only *)
let listen quad = 
  let (ip1,port1,ip2,port2) = quad in
  let listening_server = ref None in
  let server = ref None in
  let err = 
    try 
      let addr = Unix.ADDR_INET (ip1,port1) in
      let list_srvr = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let _ = listening_server := Some list_srvr in
      let _ = 
	(* hack to speed up recovery *)
	Unix.setsockopt list_srvr Unix.SO_REUSEADDR true;
	Unix.bind list_srvr addr;
	Unix.listen list_srvr 5
      in
      let (srvr,_) = Unix.accept list_srvr in
      let _ = server := Some srvr in
      None
    with e -> Some e
  in
  let _ = match !listening_server with | None -> () | Some s -> close_fd_noerr s in
  let _ = 
    match !server with 
    | None -> ()
    | Some srvr -> 
	if Unix.getpeername srvr <> Unix.ADDR_INET(ip2,port2) then 
	  (close_fd_noerr srvr; server:=None)
	else 
	  ()
  in
  let  _ = match err with | None -> () | Some e -> debug "listen, err" in
  match (err,!server) with 
  | (Some e,None) -> raise e
  | (Some e,Some srvr) -> (close_fd_noerr srvr; raise e)
  | (None,None) -> raise Exception
  | (None,Some srvr) -> srvr;;

let connect quad = 
  let client = ref None in
  let err = 
    try 
      let (ip1,port1,ip2,port2) = quad in
      let src = Unix.ADDR_INET (ip1,port1) in
      let dst = Unix.ADDR_INET (ip2,port2) in
      let c = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let _ = client := Some c in
      let _ = 
	(* hack to speed up recovery *)
	Unix.setsockopt c Unix.SO_REUSEADDR true;
	Unix.bind c src;
	Unix.connect c dst
      in
      None
    with e -> Some e
  in
  let _ = match err with | None -> () | Some e -> debug "connect, err" in
  match (err,!client) with
  | (Some e,None) -> raise Exception
  | (Some e,Some c) -> (close_fd_noerr c; raise Exception)
  | (None,None) -> raise Exception (* can't happen *)
  | (None,Some c) -> c;;

let send c ss =
  let err = 
    try
      let oc' = Unix.out_channel_of_descr c in
      let ss' = Encode2.encode_strings ss in
      (* outputs everything hopefully *)
      let _ = output_string oc' (ss'^"\n") in
      let _ = flush oc' in
      None
    with e -> Some e
  in
  let _ = match err with | None -> () | Some e -> debug "send, err" in
  match err with 
  | None -> () 
  | Some e -> (
(*      (match !oc with | None -> () | Some oc -> close_out_noerr oc); *)
(*      close_fd_noerr c; *)
      raise Exception);;

let can_recv c =
  try
    let (r,_,_) = Unix.select [c] [] [] 0.0 in
      not (r=[])
  with _ -> ((*close_fd_noerr c;*) raise Exception);;

let recv c = 
  let s = ref None in
  let err = 
    try
      let ic' = Unix.in_channel_of_descr c in
      let e = input_line ic' in
      let _ = s := Some(Encode2.decode_strings e) in
      None
    with e -> Some e
  in
  let _ = match err with | None -> () | Some e -> debug "recv, err" in
  match err with
  | None -> dest_Some(!s)
  | Some e -> (
(*      (match !ic with | None -> () | Some ic -> close_in_noerr ic); *)
(*      close_fd_noerr c; *)
      raise Exception);;

let close c = try Unix.close c with _ -> raise Exception;;

let close_noerr c = close_fd_noerr c;;
