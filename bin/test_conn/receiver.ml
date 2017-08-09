open Net_pervasive

let rq = Listen.listen_accept ~quad:Shared.recvr

let i = ref 1

let p = mk_profiler ()

let main () = 
  while true do
    try 
      assert (p.mark' P.ab);
      let msg = Send_recv.recv_string rq in 
      assert (p.mark' P.bc);
      (if !i mod 100 = 0 then print_endline msg else ());
      i:=!i+1;
      Send_recv.send_string rq msg;
      assert (p.mark' P.cd);
    with 
    | e -> (print_endline @@ __LOC__^": unknown exception"; raise e)
  done

let _ = main ()
