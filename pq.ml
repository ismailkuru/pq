(* persistent queue; currently not persistent :( *)

open Pq_pervasive
open Pq_connection

let maybe_raise e = match e with | None -> () | Some e -> raise e;;


type queue = { 
  (* shared state *)
  lock : Mutex.t ;
  cond : Condition.t ;
  msgs : (string list) ref ;
  (* active thread local state *)
  b    : bool ref ;
  fd   : conn option ref ;
  (* constant *)
  quad : quad ;
  fn   : string ;
}


let mk_queue ~quad ~fn ~is_sender = { 
  lock = Mutex.create () ;
  cond = Condition.create () ;
  msgs = ref [] ;
  b    = ref is_sender ;
  fd   = ref None ;
  quad = quad ;
  fn   = fn ;
}

(* assumes q.lock is held *)
let save (q:queue) = None
(* TODO
  let b = string_of_bool ( ! ( q.b ) ) in
  try 
    File.write q.fn ( b :: ( ! ( q.msgs ) ) ) ; None
  with 
  | File.Exception -> Some File.Exception in
*)

(* FIXME file exceptions are ignored (could be missing files) make more precise *)
let old_init q = () (* TODO 
  Mutex.lock q.lock;
  begin
    try 
      let ( b :: msgs ) = File.read q.fn in
      let _ = q.b := bool_of_string b in
      let _ = q.msgs := msgs in
      ()
    with _ -> ()
  end;
  Mutex.unlock q.lock;
  () *)

let init q = ()


(* receiver --------------------------------------------------------- *)

(* receive.broadcast/remove.wait,peek.wait *)
(* throws File.Exception Messaging.Exception *)
let private_receive q =
  let [b;msg] = recv (dest_Some !(q.fd)) in
  match bool_of_string b = not !(q.b) with
  | true -> (
    debug "receiver got a valid msg\n";
    Mutex.lock q.lock;
    q.msgs := !(q.msgs)@[msg];
    q.b := not !(q.b);
    let e = save q in
    Condition.broadcast q.cond;
    (* FIXME are we sure this above has to be inside the lock? why not in private_send? *)
    Mutex.unlock q.lock;
    maybe_raise e)
  | false -> ()[@@warning "-8"]


(* receiver.signal/listen.wait - no other threads have access yet *)
(* throws File.Exception *)
let recv_thread q = 
  while true do
    try 
      print_endline "receiver: initializing"; 
      q.fd := Some(listen q.quad);
      while true do
        private_receive q; 
        send (dest_Some !(q.fd)) [string_of_bool !(q.b)];
      done
    with 
    | Exception e -> (
        print_endline @@ __LOC__^": recv_thread: "^e;
        match !(q.fd) with 
        | None -> () 
        | Some fd ->
          close_no_err fd;
          q.fd := None)
    | e -> (
        print_endline @@ __LOC__ ^ "recv_thread: unknown exception";
        raise e)
    (*    | File.Exception -> (
            print_string "receiver: File.Exception\n" ;
            raise File.Exception ) TODO *)
  done

let available q = 
  Mutex.lock q.lock;
  let avl = !(q.msgs) <> [] in
  Mutex.unlock q.lock;
  avl

(* may block waiting for a msg to arrive *)
(* peek.wait/receive.broadcast - ok to have 2 user threads *)
let peek q = 
  Mutex.lock q.lock;
  while !(q.msgs) = [] do Condition.wait q.cond q.lock done;
  let msg = List.hd (!(q.msgs)) in 
  Mutex.unlock q.lock;
  msg

(* remove.wait/receive.broadcast *)
(* throws File.Exception *)
let remove q =
  Mutex.lock q.lock;
  while !(q.msgs) = [] do Condition.wait q.cond q.lock done;
  q.msgs := List.tl (!(q.msgs));
  let e = save q in
  Mutex.unlock q.lock;
  maybe_raise e

let listen quad fn = 
  let q = mk_queue quad fn false in
  init q;
  let t = Thread.create recv_thread q in  (* FIXME store thread with queue? *)
  q[@@warning "-26"]


(* sender ----------------------------------------------------------- *)

(* FIXME in the following change let msgs to use :: ? *)

(* locking used only for communication; note safe interference- q.msgs
   accessed outside lock (we do not require all shared state to be
   welllocked) *)
(* send_msg.wait/send.signal *)
(* throws Exception *)
let _send q =
  Mutex.lock q.lock;
  while !(q.msgs) = [] do Condition.wait q.cond q.lock done;
  Mutex.unlock q.lock;
  let msgs = [string_of_bool !(q.b); List.hd !(q.msgs)] in
  send (dest_Some !(q.fd)) msgs

(* throws File.Exception Exception *)
let _sender_recv q = 
  let msg = List.hd (recv (dest_Some (!(q.fd)))) in
  match !(q.b)  = bool_of_string msg with
  | true -> (
      Mutex.lock q.lock;
      q.msgs := List.tl !(q.msgs);
      q.b := not !(q.b);
      let e = save q in
      Mutex.unlock q.lock;
      maybe_raise e)
  | false -> ()

(* throws File.Exception *)
let sender_thread q =
  debug "sender starts\n";
  while true do 
    try
      print_endline "sender: initializing"; 
      q.fd := Some(connect q.quad); 
      while true do
        _send q;
        _sender_recv q 
      done
    with 
    | Exception e -> ( 
        print_endline @@ __LOC__^"sender_thread: "^e;
        match !(q.fd) with 
        | None -> () 
        | Some fd -> close_no_err fd; q.fd := None)
    (* debug    | File.Exception -> ( raise File.Exception ) *)
    | e -> raise e
  done

(* FIXME check that broadcast does not have to be inside the locked region *)
(* send.signal/send_msg.wait *)
(* throws File.Exception *)
let send q s = 
  Mutex.lock q.lock;
  q.msgs := !(q.msgs)@[s];
  let e = save q in
  Mutex.unlock q.lock;
  Condition.broadcast q.cond;
  maybe_raise e;
  (* sender throttling *)
  let l = List.length !(q.msgs) in
  (float_of_int l) /. 1000.0 |> fun secs ->
  Thread.delay secs

let connect quad fn = 
  debug "_connect 1\n";
  let q = mk_queue quad fn true in
  debug "_connect 2\n";
  init q;
  debug "_connect 3\n";
  let t = Thread.create sender_thread q in  (* FIXME? *)
  debug "_connect 4\n";
  q[@@warning "-26"]

