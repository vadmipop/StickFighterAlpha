(** A text object in the GUI *)

type t

val init : Yojson.Basic.t -> t
(** [init json] will initialize a text object from [json] *)

val draw : t -> Graphics.Screen.t -> unit
(** [draw text screen] will render [text] to the [screen] *)