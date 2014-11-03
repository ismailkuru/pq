exception Exception;;

type conn
type ip = Unix.inet_addr
type port = int
type quad = ip*port*ip*port
val listen: quad -> conn
val connect: quad -> conn
val send: conn -> string list -> unit
val can_recv: conn -> bool
val recv: conn -> string list
val close: conn -> unit
val close_noerr: conn -> unit
