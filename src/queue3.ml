(* N.B.!!! a deref with a record must be bracketed ! ( q.b ) for native compilation *)

(* FIXME change to support sending a list of strings- much nicer *)

(*
type rqueue = (unit -> bool) * (unit -> string) * (unit -> unit)
type squeue = string -> unit
*)
let print_string s = (print_string s; flush stdout);;
let debug = Common.debug;;
let is_Some e = match e with Some _ -> true | _ -> false;;
let dest_Some e = match e with Some e -> e | _ -> failwith "dest_Some";;
let maybe_raise e = match e with | None -> () | Some e -> raise e;;

let update w l e = (l,e)::w;;
let lookup w l = List.assoc l w;;


(******************************************************************************)
(* types and aux fns *)

type filename = string;;
type ip = Unix.inet_addr;;
type port = int;;
type quad = ip*port*ip*port;;

type queue = { 
  (* shared state *)
  lock : Mutex.t ;
  cond : Condition.t ;
  msgs : (string list) ref ;
  (* active thread local state *)
  b    : bool ref ;
  fd   : Message.conn option ref ;
  (* constant *)
  quad : quad ;
  fn   : string ;
}

(*  tid : Thread.t option; (* helper thread id *) *) 
(*  is_sender: bool *)


(******************************************************************************)
(* the code *)

(* this interface means that we don't have a global list of queues, so
we have to call connect etc. *)

type rqueue = (unit -> bool) * (unit -> string) * (unit -> unit);;
type squeue = (string -> unit);;

let ((listen:quad->filename->rqueue),
     (available:rqueue->bool),
     (peek:rqueue->string),
     (remove:rqueue->unit),
     (connect:quad->filename->squeue),
     (send:squeue->string->unit)) = 
  
let mk_queue quad fn is_sender = { 
  lock = Mutex.create () ;
  cond = Condition.create () ;
  msgs = ref [] ;
  b    = ref is_sender ;
  fd   = ref None ;
  quad = quad ;
  fn   = fn ;
} in

(*  tid = None; *)
(*  is_sender = is_sender; *)

(* assumes q.lock is held *)
let save (q:queue) = 
  let b = string_of_bool ( ! ( q.b ) ) in
  try 
    File.write q.fn ( b :: ( ! ( q.msgs ) ) ) ; None
  with 
  | File.Exception -> Some File.Exception in

(* FIXME file exceptions are ignored (could be missing files) make more precise *)
let init q = 
  let _ = Mutex.lock q.lock in
  let _ = 
    try 
      let ( b :: msgs ) = File.read q.fn in
      let _ = q.b := bool_of_string b in
      let _ = q.msgs := msgs in
      ()
    with _ -> () in
  let _ = Mutex.unlock q.lock in
  () in


(******************************************************************************)
(* receiver *)

(* receive.broadcast/remove.wait,peek.wait *)
(* throws File.Exception Messaging.Exception *)
let private_receive q =
  let [ b ; msg ] = Message.recv ( dest_Some ( ! ( q.fd ) ) ) in
  if bool_of_string b = not ( ! ( q.b ) ) then (
    let _ = debug "receiver got a valid msg\n" in
    let _ = Mutex.lock q.lock in
    let _ = q.msgs := ! ( q.msgs ) @ [ msg ] in
    let _ = q.b := not ( ! ( q.b ) ) in
    let e = save q in
    let _ = Condition.broadcast q.cond in  (* FIXME are we sure this has to be inside the lock? why not in private_send? *)
    let _ = Mutex.unlock q.lock in
    maybe_raise e
   ) else () in

(* receiver.signal/listen.wait - no other threads have access yet *)
(* throws File.Exception *)
let private_receiver q = 
  while true do
    try 
      print_string "receiver: initializing\n" ; 
      q.fd := Some ( Message.listen q.quad ) ; 
      while true do
        debug "before private_receive\n";
        private_receive q ; 
        debug "after private_receive\n";
        Message.send ( dest_Some ( ! ( q.fd ) ) ) [ string_of_bool ( ! ( q.b ) ) ];
        debug "after Message.send\n";
      done
    with 
    | Message.Exception -> (
        debug "receiver: Message.Exception\n" ;
        ( match ( ! ( q.fd ) ) with 
        | None -> () 
        | Some fd -> ( 
            ( try Message.close fd with _ -> () ) ;
            q.fd := None ) );
        () ) 
    | File.Exception -> (
        print_string "receiver: File.Exception\n" ;
        raise File.Exception )
    | e -> (
        print_string "receiver: unknown Exception\n" ;
        raise e )
  done in

let _available q = 
  let _ = Mutex.lock q.lock in
  let avl = ! ( q.msgs ) <> [] in
  let _ = Mutex.unlock q.lock in
  avl in

(* may block waiting for a msg to arrive *)
(* peek.wait/receive.broadcast - ok to have 2 user threads *)
let _peek q = 
  let _ = Mutex.lock q.lock in
  let _ = while ! ( q.msgs ) = [] do Condition.wait q.cond q.lock done in
  let msg = List.hd ( ! ( q.msgs ) ) in 
  let _ = Mutex.unlock q.lock in
  msg in

(* remove.wait/receive.broadcast *)
(* throws File.Exception *)
let _remove q =
  let _ = Mutex.lock q.lock in
  let _ = while ! ( q.msgs ) = [] do Condition.wait q.cond q.lock done in
  let _ = q.msgs := List.tl ( ! ( q.msgs ) ) in
  let e = save q in
  let _ = Mutex.unlock q.lock in
  maybe_raise e in

let _listen quad fn = 
  let q = mk_queue quad fn false in
  let _ = init q in
  let _ = Thread.create private_receiver q in
  q in


(******************************************************************************)
(* sender *)

(* FIXME in the following change let msgs to use :: ? *)

(* locking used only for communication; note safe interference- q.msgs accessed outside lock (we do not require all shared state to be welllocked) *)
(* send_msg.wait/send.signal *)
(* throws Message.Exception *)
let private_send q =
  let _ = debug "private_send: before mutex stuff\n" in
  let _ = Mutex.lock q.lock in
  let _ = 
    while ! ( q.msgs ) = [] do 
      Condition.wait q.cond q.lock 
    done 
  in
  let _ = Mutex.unlock q.lock in 
  let _ = debug "private_send: past mutex stuff\n" in
  let msgs = [ string_of_bool ( ! ( q.b ) ) ; 
               List.hd ( ! ( q.msgs ) ) ] in
  let _ = Message.send ( dest_Some ( ! ( q.fd ) ) ) msgs in
  () in

(* throws File.Exception Message.Exception *)
let private_recv q = 
  let msg = List.hd ( Message.recv ( dest_Some ( ! ( q.fd ) ) ) ) in
  if ! ( q.b )  = bool_of_string msg then (
    let _ = Mutex.lock q.lock in
    let _ = q.msgs := List.tl ( ! ( q.msgs ) ) in
    let _ = q.b := not ( ! ( q.b ) )  in 
    let e = save q in
    let _ = Mutex.unlock q.lock in
    maybe_raise e
   ) else () in

(* throws File.Exception *)
let sender q =
  debug "sender starts\n";
  while true do 
    try
      print_string "sender: initializing\n"; 
      q.fd := Some ( Message.connect q.quad ) ; 
      debug "sender: post initializing\n"; 
      while true do
        debug "sender: top of while loop\n"; 
        private_send q ;
        debug "sender: after private_send\n"; 
        private_recv q 
      done
    with 
    | Message.Exception -> ( 
        match ! ( q.fd ) with 
        | None -> () 
        | Some fd -> ( Message.close_noerr fd ; q.fd := None ) )
(* debug    | File.Exception -> ( raise File.Exception ) *)
    | e -> ( raise e )
  done in

(* FIXME check that broadcast does not have to be inside the locked region *)
(* send.signal/send_msg.wait *)
(* throws File.Exception *)
let _send q s = 
  let _ = Mutex.lock q.lock in
  let _ = q.msgs := ( ( ! ( q.msgs ) ) @ [ s ] ) in
  let e = save q in
  let _ = Mutex.unlock q.lock in
  let _ = Condition.broadcast q.cond in
  let _ = maybe_raise e in
  let l = List.length ( ! ( q.msgs ) ) in
(*  let l = 0 in *)
  Thread.delay ( ( float_of_int l ) /. 1000.0 ) in

let _connect quad fn = 
  let _ = debug "_connect 1\n" in
  let q = mk_queue quad fn true in
  let _ = debug "_connect 2\n" in
  let _ = init q in
  let _ = debug "_connect 3\n" in
  let _ = Thread.create sender q in 
  let _ = debug "_connect 4\n" in
  q in

(******************************************************************************)

let listen quad fn =
  let q = _listen quad fn in
  let available () = _available q in
  let peek () = _peek q in
  ((fun u -> _available q),(fun u -> _peek q),fun u -> _remove q) in

let available (a,p,r) = a () in
let peek (a,p,r) = p () in
let remove (a,p,r) = r () in

let connect quad fn = 
  let _ = debug "connect\n" in
  let q = _connect quad fn in
  _send q in

let send q s = q s in

  (* what is bound for the rest of the code *)
  (listen,available,peek,remove,connect,send);;
