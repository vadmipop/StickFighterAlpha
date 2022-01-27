(** A rectangle object in hte GUI *)

type t

val init : Yojson.Basic.t -> t
(** [init json] will initialize a rect from [json] *)

val moveto : t -> int * int -> unit
(** [moveto rect (x, y)] will reposition [rect] to position [(x, y)] *)

val draw : t -> Graphics.Screen.t -> unit
(** [draw rect screen] will render [rect] to the [screen] *)