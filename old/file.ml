(* 

This is a dummy implementation.

assumptions: strings do not contain "\n"

*)

(*

sender error trace

sender: initializing
message.ml/connect: exception
sender: initializing
file.ml/write, exception
sender.ml: unknown exception
Fatal error: exception Sys_error("Bad file descriptor")

running out of file descriptors? typically sender.tmp.tmp is empty, i.e. was just about to write to it

another sender error trace

sender: initializing
message.ml/connect, err
sender: initializing
message.ml/connect, err
file.ml/write, exception
sender.ml: unknown exception
sender: initializing
Fatal error: exception Sys_error("Broken pipe")


message.ml/connect, err                                                                                                      
sender: initializing                                                                                                         
message.ml/connect, err                                                                                                      
sender: initializing                                                                                                         
file.ml/write, exception                                                                                                     
sender.ml: unknown exception                                                                                                 
Fatal error: exception Sys_error("Broken pipe")                                                                              

sender: initializing
message.ml/connect, err
18421 10974
sender: initializing
file.ml/write, before output_string
message.ml/connect, err
file.ml/write, after output_string
sender: initializing
file.ml/write, exception
sender.ml: unknown exception
Fatal error: exception Sys_error("Bad file descriptor")

message.ml/connect, err
sender: initializing
message.ml/connect, err
sender: initializing
message.ml/connect, err
18725 4600
sender: initializing
file.ml/write, before output_string
message.ml/connect, err
file.ml/write, after output_string
sender: initializing
file.ml/write, exception
sender.ml: unknown exception
message.ml/connect, err
sender: initializing
Fatal error: exception Sys_error("Bad file descriptor")

23624 6117                                                                                                                   
message.ml/connect, err                                                                                                      
sender: initializing                                                                                                         
message.ml/connect, err                                                                                                      
file.ml/write, before output_string                                                                                          
sender: initializing                                                                                                         
file.ml/write, after output_string                                                                                           
message.ml/connect, err                                                                                                      
sender: initializing                                                                                                         
file.ml/write, exception                                                                                                     
sender.ml: unknown exception                                                                                                 
Fatal error: exception Sys_error("Broken pipe")                                                                              

if we ulimit -n 32, errors occur almost immediately, so we are running
out of file descriptors on the sender somehow? but when listing these
they remain fairly constant...

FIXME we should only ever close a fd once!

*)

let print_string s = (print_string s; flush stdout);;
let debug s = Common.debug ("file.ml/"^s^"\n");;
(* let debug s = ();; *)

exception Exception;;

let delete fn = 
  try 
    Unix.unlink fn 
  with Unix.Unix_error(x,y,z) -> 
    (debug "delete, err"; ());;

let write fn ss =
  let fn' = fn^".tmp" in
  (* may not be present *)
  let _ = try Unix.unlink fn' with Unix.Unix_error _ -> () in 
  let oc = ref None in
  let err = 
    try 
      let oc' = open_out fn' in
      let _ = oc := Some oc' in
      let ss' = Encode2.encode_strings ss in
      let _ = debug "write, before first flush" in
      let _ = flush oc' in
      let _ = debug "write, before output_string" in
      let _ = output_string oc' (ss'^"\n") in
      let _ = debug "write, after output_string" in
      (* the following is causing the problems... *)
      let _ = flush oc' in
      let _ = debug "write, after flush" in
(*      let _ = close_out_noerr oc in FIXME FIXME FIXME *)
      let _ = debug "write, after close_out_noerr" in
      None
    with e -> Some e
  in
  let _ = match !oc with None -> () | Some oc -> close_out_noerr oc in
  match err with 
  | None -> (Unix.handle_unix_error (Unix.rename fn') fn)
  | Some e -> (debug "write, exception"; raise e);;

let read fn =
  let cin = ref None in
  let s = ref None in
  let err = 
    try 
      (* read whole file as a single string - assume no returns*)
      let cin' = open_in fn in 
      let _ = cin := Some cin' in
      let str = input_line cin' in
      let str' = Encode2.decode_strings str in
      let _ = s := Some str' in
      None
    with e -> Some e
  in
  let _ = match !cin with | None -> () | Some c -> close_in_noerr c in
  let _ = match err with | None -> () | Some e -> debug "read, err" in
  match !s with 
  | None -> raise Exception
  | Some s -> s;;
