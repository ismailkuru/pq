open Pq_pervasive
open Pq
open Shared

let rq = Pq.listen (Shared.ip,Shared.rport,Shared.ip,Shared.sport) recvr_fn

let i = ref 1

let main () = 
  while true do
    try 
      let s = peek rq in
      print_string (s^"\n");
      remove rq;
(*      i:=!i+1;
      if !i=100 then (print_profile (Pq.recv_thread_p.get()); failwith __LOC__) else ()*)
    with 
    (*| File.Exception -> print_string "receiver.ml: File.exception\n" *)
    | e -> (print_string "receiver.ml: unknown exception\n"; raise e)
  done


let _ = main ()
