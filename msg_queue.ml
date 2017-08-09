(* message queue over sockets *)

(* FIXME occasionally the sender disappears, but the receiver doesn't
   reinit till it reaches the end of the data (the stream isn't
   detected as disconnected by the receiver; is this a bug or a
   feature? this is a bit like a "normal" close *)

open Net_pervasive
open Send_recv

module Internal_ = struct

  type queue = { 
    (* shared state *)
    lock : Mutex.t;
    cond : Condition.t;
    msgs : (string list) ref;
    (* active thread local state *)
    fd   : conn option ref;
    (* constant *)
    quad: quad;
  }


  let mk_queue ~quad = { 
    lock = Mutex.create ();
    cond = Condition.create ();
    msgs = ref [];
    fd   = ref None;
    quad = quad;
  }


  (* receiver --------------------------------------------------------- *)

  let p = mk_profiler ()
  let private_receive_p = p

  (* receive.broadcast/remove.wait,peek.wait *)
  (* throws File.Exception Messaging.Exception *)
  let _receive q =
    recv_string (dest_Some !(q.fd)) |> fun msg ->
    Mutex.lock q.lock;
    q.msgs := !(q.msgs)@[msg];
    Condition.broadcast q.cond;
    Mutex.unlock q.lock


  let p = mk_profiler ()
  let recv_thread_p = p

  (* receiver.signal/listen.wait - no other threads have access yet *)
  (* throws File.Exception *)
  let recv_thread q = 
    while true do
      try 
        print_endline "receiver: initializing"; 
        q.fd := Some(Listen.listen_accept q.quad);
        while true do
          _receive q;
        done
      with 
      | Pq_exc e -> (
          print_endline @@ __LOC__^": recv_thread: "^e;
          match !(q.fd) with 
          | None -> () 
          | Some fd ->
            pq_close_noerr fd;
            q.fd := None)
      | e -> (
          print_endline @@ __LOC__ ^ "recv_thread: unknown exception";
          raise e)
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
    Mutex.unlock q.lock

  let listen ~quad = 
    let q = mk_queue quad in
    let t = Thread.create recv_thread q in  (* FIXME store thread with queue? *)
    q[@@warning "-26"]


  (* sender ----------------------------------------------------------- *)

  (* locking used only for communication; note safe interference- q.msgs
     accessed outside lock (we do not require all shared state to be
     welllocked); FIXME moved "let msgs" inside lock for time being *)
  (* send_msg.wait/send.signal *)
  (* throws Exception *)
  let _send q =
    Mutex.lock q.lock;
    (* FIXME possible that the internal thread gets stuck here? but then
       attempting to send another msg should wake the internal thread *)
    while !(q.msgs) = [] do Condition.wait q.cond q.lock done;
    let msg = List.hd !(q.msgs) in
    q.msgs:=List.tl !(q.msgs);
    Mutex.unlock q.lock;
    send_string (dest_Some !(q.fd)) msg


  let p = mk_profiler ()
  let sender_thread_p = p

  (* throws File.Exception *)
  let sender_thread q =
    while true do 
      try
        print_endline "sender: initializing"; 
        q.fd := Some(Connect.connect q.quad); 
        while true do
          _send q
        done
      with 
      | Pq_exc e -> ( 
          print_endline @@ __LOC__^"sender_thread: "^e;
          match !(q.fd) with 
          | None -> () 
          | Some fd -> pq_close_noerr fd; q.fd := None)
      | e -> (
          print_endline @@ __LOC__^"sender_thread, unknown exception: "^(Printexc.to_string e);
          raise e)
    done

  (* FIXME check that broadcast does not have to be inside the locked region *)
  (* send.signal/send_msg.wait *)
  (* throws File.Exception *)
  let send q s = 
    Mutex.lock q.lock;
    q.msgs := !(q.msgs)@[s];
    let l = List.length !(q.msgs) in
    Condition.broadcast q.cond;
    Mutex.unlock q.lock;
    (* sender throttling; allow sender to run 1000 msgs in front *)
    if l > 1000 then 
      (float_of_int l) /. 10000.0 |> fun secs ->
      Thread.delay secs (* FIXME *)
    else ()

  let connect ~quad = 
    let q = mk_queue ~quad in
    let t = Thread.create sender_thread q in 
    q[@@warning "-26"]

end

(* receiver *)
module R = struct
  open Internal_
  let (listen,available,peek,remove) = (listen,available,peek,remove)
end

module S = struct
  open Internal_
  let (connect,send) = (connect,send)
end
