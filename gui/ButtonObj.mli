(** A clickable button object for the GUI *)

type t

val init : Yojson.Basic.t -> t
(** [init json] will create a button using the input json. Requires:
    json follows the proper format, and has the necessary fields *)

val onclick : t -> string
(** [onclick button] will return the name of the function to be
    performed when the button is clicked*)

val update : t -> (int * int) * Sdlmouse.button list -> bool
(** [update buttom mouse] will update the button, and return whether is
    has been clicked*)

val draw :
  t -> Graphics.Screen.t -> (int * int) * Sdlmouse.button list -> unit
(** [draw button screen mouse] will draw the button *)
