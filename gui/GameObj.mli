(** Controls / draws the game in the GUI, and performs any necessary
    updates *)

type t

val init : Yojson.Basic.t -> float * float -> t
(** [init json dims] will initialize a new GameObj with the specified
    json and dimensions *)

val reset : t -> unit
(** [reset game] will reset the specified game object *)

val bind : t -> (t -> unit) -> unit
(** [bind game f] will set a new function [f] to be run when a player
    wins *)

val continue_button : t -> string
(** [continue_button game] will get the name of the game's continue
    button *)

val update : t -> Game.KeyReader.t -> unit
(** [update game keys] will update [game] with the specified [keys] *)

val draw : t -> Graphics.Screen.t -> unit
(** [draw game screen] will render the game to the screen *)