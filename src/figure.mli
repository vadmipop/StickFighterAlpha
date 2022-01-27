(** Holds a player, and deals with performing updates and actions on
    this player. *)

type t

val update : t -> Terrain.t -> t
(** [update] updates all of the figures in the game with any changes
    that have happened since the last update *)

val add_force : t -> Vector.t -> t
(** [add_force] returns a copy of a figure with an added force *)

val jump : t -> Vector.t -> t
(** [jump] will cause a figure to jump. If the figure is not standing on
    a polygon, it will not be able to jump *)

val apply_move : t -> string -> Direction.dir -> t option
(** [move] will move a figure by the distances in the vector inputted *)

val add_grav : t -> t
(** [ad_grav] will add a gravitational force to the specified figure. *)

val speed : t -> float
(** [speed] will return the current speed the figure is moving at *)

val bound_speed : t -> float -> float -> t
(** [bound_speed] will set an upper bound on the speed in the x and y
    direction that a figure can reach respectively *)

val get_hitboxes : t -> RigidPoly.t list
(** [get_hitboxes] will return the hitboxes of a figure *)

val get_name : t -> string
(** [get_name] will return the name of a figure *)

val get_force : t -> Vector.t
(** [get_force fig] will return the force currently applied to [fig] *)

val get_pos : t -> Vector.t
(** [get_pos fig] is the position of [fig] *)

val get_move : t -> Move.t
(** [get_move fig] is the current move of [fig] *)

val advance_move : t -> t
(** [advance_move fig] will advance [fig]'s move one step *)

val dir_facing : t -> bool
(** [dir_facing fig] is [true] if [fig] is moving in the positive x
    direction, [false] otherwise*)

val init_from_json : string -> Vector.t -> Yojson.Basic.t -> t
(** init_from_json will take a character .json file and return the
    character as a figure *)

val get_lines : t -> (Vector.t * Vector.t) list
(** [get_lines fig] returns a list of lines composing [fig]'s body *)

val on_ground : t -> bool
(** Returns true if the figure is on ground and false otherwise.*)

val init_empty_figure : unit -> t
(** Initializes an empty figure*)

val add_damage : t -> float -> t
(** [add_damage fig dmg] will add damage [dmg] to [fig] *)

val get_damage : t -> float
(** [get_damage fig] is the current damage done to [fig] *)

val get_color : t -> int * int * int
(** [get_color fig] returns the color of fig *)