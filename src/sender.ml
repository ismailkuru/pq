let print_string s = (print_string s; flush stdout);;

let sq = Queue3.connect (Common.ip,Common.sport,Common.ip,Common.rport) "sender.tmp";;

(* let _ = print_string "sender/got here\n";; *)

(* while true do () done;;  *)
(*Thread.delay 1.0;;*)

let pid = string_of_int (Unix.getpid ());;

let i = ref 1;;

while true do
  let msg = pid ^" "^(string_of_int !i) in
  let _ = print_string (msg^"\n") in
  let _ = (try Queue3.send sq msg with 
	     | File.Exception -> (print_string "sender.ml: File.Exception\n"; raise File.Exception)
	     | e -> (print_string "sender.ml: unknown exception\n"; raise  e) ) 
  in
    i := !i + 1
done;;
