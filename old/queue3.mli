type filename = string
type ip = Unix.inet_addr
type port = int
type quad = ip*port*ip*port

type rqueue (* = (unit -> bool) * (unit -> string) * (unit -> unit) *)
type squeue (* = string -> unit *)

val listen    : quad -> filename -> rqueue
val available : rqueue -> bool
val peek      : rqueue -> string
val remove    : rqueue -> unit
val connect   : quad -> filename -> squeue
val send      : squeue -> string -> unit
