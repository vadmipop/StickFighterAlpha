(** A line object in the GUI *)

type t

val init : Yojson.Basic.t -> t
(** [init json] will initialize a line from [json] *)

val draw : t -> Graphics.Screen.t -> unit
(** [draw line screen] will render [line] to the [screen] *)