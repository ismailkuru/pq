open Net_pervasive

let sq = Connect.connect ~quad:Shared.sender

let pid = Unix.getpid () |> string_of_int

let i = ref 1

let p = mk_profiler ()

let main () = 
  while true do
    (* dummy message: pid and a counter *)
    let msg = pid ^" "^(string_of_int !i) in
    (if !i mod 100 = 0 then print_endline msg else ());
    assert (p.mark' P.ab);
    Send_recv.send_string sq msg;
    assert (p.mark' P.bc);
    ignore (Send_recv.recv_string sq);  (* ! *)
    assert (p.mark' P.cd);
    i := !i + 1;
  done

let _ = main()
