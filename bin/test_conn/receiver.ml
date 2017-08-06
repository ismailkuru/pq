open Pq_pervasive
open Pq_connection
open Shared

let rq = listen_accept (Shared.ip,Shared.rport,Shared.ip,Shared.sport) 

let i = ref 1

let main () = 
  while true do
    try 
      let msg = recv_string rq in
      (if !i mod 100 = 0 then print_endline msg else ());
      i:=!i+1;
      (* ping it back *)
      send_string rq msg;
(*
      if !i=100 then (print_profile (Pq.recv_thread_p.get()); failwith __LOC__) else ()*)
    with 
    (*| File.Exception -> print_string "receiver.ml: File.exception\n" *)
    | e -> (print_endline @@ __LOC__^": unknown exception"; raise e)
  done

let _ = main ()
