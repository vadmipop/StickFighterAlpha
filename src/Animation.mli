(** An animation object, holding all location information for the
    "joints" in a figures body over the course of an animation *)

type t

val init : Yojson.Basic.t -> t
(** [init json] initializes an animation using [json] *)

val get_point : string -> t -> Vector.t option
(** [get_point id animation] is [Some v] where v is the point with the
    specified [id]. If no such point exists, returns [None] *)

val advance : t -> t option
(** [advance animation] is [Some animation2] if [animation] loops, or
    has more frames. Otherwise, returns [None]*)

val reset : t -> t
(** [reset animation] will return an animation that has been completely
    reset, as if it was freshly initialized *)

val reflect : t -> t
(** [reflect animation] will return an animation that had been reflected
    over the vertical axis, but has all other values the same *)
