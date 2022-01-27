(** Controls the movements of the CPU in the game *)

type t

val init : Figure.t -> t
(** [init fig] will initialize the cpu for [fig] *)

val update : Engine.t -> Engine.t
(** [update eng] will perform any updates to the engine involving the
    CPU's movement and actions *)

val is_cpu : Figure.t -> bool
(** [is_cpu fig] returns whether [fig] has an attached CPU *)