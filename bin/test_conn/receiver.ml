open Pq_pervasive
open Pq_connection
open Shared

let rq = listen_accept (Shared.ip,Shared.rport,Shared.ip,Shared.sport) 

let i = ref 1

let p = mk_profiler ()

let main () = 
  while true do
    try 
      assert (p.mark' P.ab);
      (* while(not (can_recv rq)) do Thread.delay 0.0001 done; *)
      let msg = recv_string rq in  (* ! *)
      assert (p.mark' P.bc);
      (if !i mod 100 = 0 then print_endline msg else ());
      i:=!i+1;
      (* ping it back *)
      send_string rq msg;
      assert (p.mark' P.cd);
      (* if !i=200 then (print_profile (Pq_connection.unix_read_p.get()); failwith __LOC__) else () *)
    with 
    (*| File.Exception -> print_string "receiver.ml: File.exception\n" *)
    | e -> (print_endline @@ __LOC__^": unknown exception"; raise e)
  done

let _ = main ()
