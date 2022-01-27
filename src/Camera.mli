(** The camera, controlling how the screen adjusts to follow the
    specified target *)

type t

val init :
  Vector.t -> float * float -> float * float -> float * float -> t
(** [init pos dims xbounds ybounds] will initialize a camera at [pos],
    with it's future positions bounded by [xbounds] and [ybounds] *)

val update : t -> Vector.t -> t
(** [update cam target] is an updated camera, following [target] *)

val coords : t -> Vector.t -> Vector.t
(** [coords cam screen_dims] is the coordinates of [cam] in a screen
    with dimensions [screen_dims] *)