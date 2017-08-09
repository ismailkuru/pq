(* open Net_pervasive*)
open Msg_queue.R

let rq = listen ~quad:Shared.recvr

let i = ref 1

let main () = 
  while true do
    let s = peek rq in
    (if (!i mod 100 = 0) then print_endline s else ());
    remove rq;
    i:=!i+1;
  done


let _ = main ()
