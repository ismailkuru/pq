open Pq_pervasive
open Pq
open Shared

let sq = connect (Shared.ip,Shared.sport,Shared.ip,Shared.rport) sender_fn

let pid = Unix.getpid () |> string_of_int

let i = ref 1

let main () = 
  while true do
    (* dummy message: pid and a counter *)
    let msg = pid ^" "^(string_of_int !i) in
    if !i mod 100 = 0 then print_endline msg else ();
    begin
      try send sq msg with 
	    (*| File.Exception -> (print_string "sender.ml: File.Exception\n"; raise File.Exception) *)
	    | e -> (print_string "sender.ml: unknown exception\n"; raise e)
    end;
    i := !i + 1;
(*    if !i = 10000 then
      (print_profile (Pq.sender_thread_p.get()); failwith __LOC__) else ()      *)
  done

let _ = main()
