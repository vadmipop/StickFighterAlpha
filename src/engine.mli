(** Holds the entire engine and performs all necessary updates on the
    figures, the stage, etc. each frame *)

type t

type player =
  | Alive of Figure.t
  | Dead of Figure.t

val player_file : string ref
(** [player_file] is the path to the file containing the player. Used
    when initializing the engine. *)

val level_file : string ref
(** [level_file] is the path to the file containing the level. Used when
    initializing the engine. *)

val init : float * float -> t
(** [init file] initializes the game state from a JSON file located at
    [file]. Requires: [file] is a valid file in the expected format *)

val update : t -> t
(** [update eng] updates the current state of the game given by [eng].*)

val get_figures : t -> player list
(** [get_figures eng] is the list of figures in the game state given by
    [eng].*)

val get_terrain : t -> Terrain.t
(** [get_terrain eng] is the terrain in the game state given by [eng].*)

val get_camera : t -> Camera.t
(** [get_camera eng] will return the Camera used by [eng] *)

val update_figures : t -> player list -> t
(** [update_figures eng lst] replaces the current list of figures in
    [eng] by [lst].*)

val update_figure : t -> Figure.t -> t
(** [update_figures eng fig] is a replica of the engine, with only the
    single figure changed (determined by having the same name) *)

val get_figure : t -> string -> player option
(** [get_figure eng name] returns a [Some player] if [player] has name
    [name]. If no such players exist, returns [None]*)

val check_winner : t -> Figure.t option
(** [check_winner eng] returns [Some player] if [player] is the only
    figure left alive. Else, returns [None]. *)
