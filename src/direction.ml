(** Holds a direction, either Left, Right, or Any *)

(** A type representing a direction *)
type dir =
  | Left
  | Right
  | Any

(** [of_string s] converts string [s] to a direction *)
let of_string s =
  match s with
  | "left" -> Left
  | "right" -> Right
  | _ -> Any

(** [to_string d] converts direction [d] to a string *)
let to_string d =
  match d with
  | Left -> "left"
  | Right -> "right"
  | Any -> "any"

(** [flip d] reflects the direction of [d] *)
let flip d =
  match d with
  | Left -> Right
  | Right -> Left
  | Any -> Any