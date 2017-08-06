open Pq_pervasive
open Pq
open Shared

let rq = Pq.listen (Shared.ip,Shared.rport,Shared.ip,Shared.sport) recvr_fn

let i = ref 1

let main () = 
  while true do
    let s = peek rq in
    (if (!i mod 100 = 0) then print_endline s else ());
    remove rq;
    i:=!i+1;
    (*if !i=100 then (print_profile (Pq.recv_thread_p.get()); failwith __LOC__) else ()*)
  done


let _ = main ()
