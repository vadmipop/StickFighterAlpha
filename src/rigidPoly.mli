(** The basic polygon class underlying all collisions and hitboxes *)

type t

val init : Vector.t list -> Vector.t -> t
(** Initialize new rigid poly with the set of points and given center.
    Requires the points are in clockwise order around the center, and
    define a convex polygon.*)

val init_rect : float -> float -> Vector.t -> t
(** Initiliaze new rectangular rigid poly with the set width and height,
    and given center *)

val center : t -> Vector.t
(** Returns the center of this rigid poly. *)

val point_in : t -> Vector.t -> bool
(** Returns whether the given point is inside this poly *)

val side_penetration : t -> t -> float * Vector.t
(** [side_penetration poly1 poly2] returns the penetration of poly1 into
    poly2, along with the normal to the side of penetration *)

val overlap : t -> t -> bool
(** Returns whether these polies overlap *)

val overlap_dir : t -> t -> Vector.t
(** Returns overlap direction, scaled by length of overlap *)

val nearest : t -> t -> float * Vector.t
(** [nearest poly1 poly2] returns the nearest distance normal to a side
    from poly1 to poly2, as well as the normal vector of the side. *)

val vertices : t -> Vector.t list
(** Returns vertices of polygon *)

val move : Vector.t -> t -> t
(** Moves the polygon by amount *)

val move_to : Vector.t -> t -> t
(** Moves the polygon to location *)

val reflect : t -> t
(** [reflect poly] returns a version of [poly] that has been reflected
    over a vertical axis *)

val average_center : t list -> Vector.t
(** [average_center polys] returns the combined center of all RigidPolys
    in [polys] *)
