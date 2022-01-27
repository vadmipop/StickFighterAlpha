(** Holds the level, specifically all polygons and platforms *)

type t

val init : RigidPoly.t list -> t
(** [init polys] will return a new terrain with all the given polygons
    included.*)

val add_body : t -> RigidPoly.t -> t
(** [add_body ter poly] will return a new terrain identical to ter, with
    poly added.*)

val get_polys : t -> RigidPoly.t list
(** [get_polys ter poly] Will return a list of all polygons currently in
    the terrain.*)
