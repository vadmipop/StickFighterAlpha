(** A basic 2D vector class for all necessary operations on vectors *)

type t

val init : float -> float -> t
(** [init x y] will initialize a vector with x-coordinate x and
    y-coordinate y*)

val from_json : Yojson.Basic.t -> t
(** [from_json json] will read a vector from the provided json *)

val x : t -> float
(** [x v] returns the x coordinate of vector [v] *)

val y : t -> float
(** [y v] returns the y coordinate of vector [v] *)

val zero : t
(** [zero] is the zero vector *)

val add : t -> t -> t
(** [add v1 v2] will add vectors [v1] and [v2] *)

val sub : t -> t -> t

val scale : float -> t -> t
(** [scale c v] will scale [v] by length [c] *)

val dot : t -> t -> float
(** [dot v1 v2] will dot vectors [v1] and [v2] *)

val rotate : float -> t -> t
(** [rotate theta v] will rotate vector [v] clockwise by [theta] radians *)

val length : t -> float
(** [length v] will calculate the length of [v] *)

val length2 : t -> float
(** [length2 v] will calculate the length squared of [v] *)

val unitize : t -> t
(** [unitize v] will find the unit vector of the same direction, and
    unit length *)

val norm : t -> t
(** [norm v] will find the vector perpendicular, of unit length*)

val make_length : float -> t -> t
(** [make_length c v] will scale [v] to have length [c]. Negative
    lengths will flip the direction of v. *)

val constrain_length : float -> float -> t -> t
(** [constrain_length l r v] will constrain [v] to have a length in (l,
    r), while keeping its direction the same. Requires l <= r*)

val constrain_xy : float * float -> t -> t
(** [constrain_xy x y] will constrain the vector to a maximum absolute
    value of [x] in its x coordinate and [y] in its y coordinate. will
    adjust the current x and y values as needed. *)

val to_string : t -> string
(** [to_string v] will output [v] in an easier to read / print string
    format *)

val to_ints : t -> int * int
(** [to_ints v] will return a tuple of [v] with each coordinate rounded
    to integers *)

val equal : t -> t -> bool
(** [equal v1 v2] will return true if v1 and v2 have the same x and y
    coords *)

val reflect : t -> t
(** [reflect v] returns a reflected copy of [v]. If [v] is a vector with
    components <x, y>, then [reflect v] is a vector with components <-x,
    y> *)
