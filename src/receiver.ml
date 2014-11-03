let print_string s = (print_string s; flush stdout);;

let rq = Queue3.listen (Common.ip,Common.rport,Common.ip,Common.sport) "receiver.tmp";;

while true do
  try 
    let s = Queue3.peek rq in
    let _ = print_string (s^"\n") in
      Queue3.remove rq
  with 
    | File.Exception -> print_string "receiver.ml: File.exception\n"
    | e -> (print_string "receiver.ml: unknown exception\n"; raise e)
done;;
