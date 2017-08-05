open Pq_pervasive
open Pq
open Shared

let rq = Pq.listen (Shared.ip,Shared.rport,Shared.ip,Shared.sport) recvr_fn

let main () = 
  while true do
    try 
      let s = peek rq in
      print_string (s^"\n");
      remove rq
    with 
    (*| File.Exception -> print_string "receiver.ml: File.exception\n" *)
    | e -> (print_string "receiver.ml: unknown exception\n"; raise e)
  done


let _ = main ()
