(** Uses a KeyReader to perform input on the characters / engine
    acclrding to what keys are pressed *)

type t

val init : string -> t
(** [init] initializes user input when the game is started.*)

val handle_input : t -> KeyReader.t -> Engine.t -> Engine.t
(** [handle_input input keys eng] is a tuple representing currently
    pressed keys (first entry) and the game engine updated according to
    keys pressed.*)
