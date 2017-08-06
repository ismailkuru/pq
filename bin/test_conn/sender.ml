open Pq_pervasive
open Pq_connection
open Shared

let sq = connect (Shared.ip,Shared.sport,Shared.ip,Shared.rport) 

let pid = Unix.getpid () |> string_of_int

let i = ref 1

let main () = 
  while true do
    (* dummy message: pid and a counter *)
    let msg = pid ^" "^(string_of_int !i) in
    (if !i mod 100 = 0 then print_endline msg else ());
    send_string sq msg;
    (* and read it back *)
    ignore (recv_string sq);
    i := !i + 1;
(*    if !i = 10000 then
      (print_profile (Pq.sender_thread_p.get()); failwith __LOC__) else ()      *)
  done

let _ = main()
