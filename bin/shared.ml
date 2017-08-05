(* some particular settings for testing *)

let ip = Unix.inet_addr_of_string "127.0.0.1"
let rport=4001
let sport=4007

let sender_fn = "sender.tmp"
let recvr_fn = "recvr.tmp"

let _ = Sys.signal Sys.sigpipe Sys.Signal_ignore

(* FIXME is this needed? if receiver is not present we get Fatal
   error: exception Sys_error("Bad file descriptor") sometimes *)
(*
let _ = Sys.signal Sys.sigabrt Sys.Signal_ignore
let _ = Sys.signal Sys.sighup Sys.Signal_ignore
let _ = Sys.signal Sys.sigchld Sys.Signal_ignore
let _ = Sys.signal Sys.sigttin Sys.Signal_ignore
*)
