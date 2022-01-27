(** Holds information regarding the moves / attacks available to a
    player, such as how they are applied, their animations, etc. *)

type t

val get_name : t -> string
(** [get_name move] returns the name of [move] *)

val get_effect :
  t * Vector.t -> t * Vector.t -> float * Vector.t * Vector.t
(** [get_effect (move1, pos1) (move2, pos2)] takes in the current move
    and position of two figures, and returns the damage, knockback, and
    polygonal penetration on the second figure *)

val get_kb : t -> Vector.t
(** [get_kb move] returns the current knockback of [move] *)

val get_hurtboxes : t -> RigidPoly.t list
(** [get_hurtboxes move] returns a list of the offensive polygons of
    [move] *)

val get_defense_boxes : t -> RigidPoly.t list
(** [get_defense_boxes move] returns a list of the defensive polygons of
    [move] *)

val get_neutral_boxes : t -> RigidPoly.t list
(** [get_neutral_boxes move] returns a list of the neutral polygons of
    [move] *)

val get_animation : t -> Animation.t
(** [get_animation move] returns the Animation associated with [move] *)

val get_where : t -> string
(** [get_where move] returns a string representing where [move] can be
    performed by the figure *)

val get_force : t -> Vector.t
(** [get_force move] returns the force that should be applied to the
    figure performing move *)

val additive_force : t -> bool * bool
(** [additive_force move] returns a tuple representing whether the move
    should be additive to the current velocity in the x and y
    directions, or replace them *)

val get_changeable : t -> bool
(** [get_changeable move] returns whether the move is interuptible by
    another move *)

val init_from_json : Yojson.Basic.t -> t
(** [init_from_json json] will return a new move with all traits
    inherited from it's json *)

val reset : t -> t
(** [reset move] will reset all variable settings of a move, to put it
    back in its original state *)

val advance : t -> t option
(** [advance move] returns [Some move'] if move' is move updated by one
    frame. If no such [move'] exists, returns [None]. *)

val advance_reset : t -> string -> t
(** [advance_reset move loc] will advance any variables associated with
    reseting the move, such as a frame counter and a terrain checker.
    [loc] is the current type of terrain of the Figure.*)

val ready : t -> bool
(** [ready move] returns [true] if the move is ready to be performed,
    [false] otherwise. *)

val not_ready : t -> t
(** [not_ready move] returns a new Move that has not been fully reset,
    and can't yet be performed. *)

val of_direction : Direction.dir -> t -> t
(** [of_direction dir move] returns a new move in the specified
    direction, by reflecting any necessary components. *)

val get_controlled : t -> bool
(** [get_controlled move] returns whether the move should have
    controlled (bounded) velocity or uncontrolled velocity *)
