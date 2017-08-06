open Pq_pervasive
open Pq_connection
open Shared

let sq = connect (Shared.ip,Shared.sport,Shared.ip,Shared.rport) 

let pid = Unix.getpid () |> string_of_int

let i = ref 1

let p = mk_profiler ()

let main () = 
  while true do
    (* dummy message: pid and a counter *)
    let msg = pid ^" "^(string_of_int !i) in
    (if !i mod 100 = 0 then print_endline msg else ());
    assert (p.mark' P.ab);
    send_string sq msg;
    assert (p.mark' P.bc);
    (* and read it back *)
    (* while(not (can_recv sq)) do Thread.delay 0.000001 done; *)
    ignore (recv_string sq);  (* ! *)
    assert (p.mark' P.cd);
    i := !i + 1;
(*    if !i = 201 then
      (print_profile (p.get()); failwith __LOC__) else () *)
  done

let _ = main()
