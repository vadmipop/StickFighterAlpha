(** Records the current keys being pressed, and discards them upon
    release *)

type t

val init : unit -> t
(** [init ()] creates a new KeyReader in which no keys are yet recorded *)

val update_keys : t -> unit
(** [update_keys keys] will add any pressed keys, and remove unpressed
    keys *)

val remove_keys : t -> string list -> unit
(** [remove_keys keys ids] will "unpress" all keys in the list [ids] *)

val check_keys : t -> string list -> bool
(** [check_keys keys ids] is [true] if all keys in [ids] are currently
    pressed, [false] otherwise *)

val check_quit : t -> bool
(** [check_quit keys] is [true] *)