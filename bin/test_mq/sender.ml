open Msg_queue.S

let sq = connect ~quad:Shared.sender

let pid = Unix.getpid () |> string_of_int

let i = ref 1

let main () = 
  while true do
    let msg = pid ^" "^(string_of_int !i) in
    if !i mod 100 = 0 then print_endline msg else ();
    send sq msg;
    i := !i + 1;
  done

let _ = main()
