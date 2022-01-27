(** A module to draw text on the screen, as there are no predefined text
    functions in ocamlsdl2 *)

type font

val make_font : float -> font
(** Will make a font of the specified size *)

val draw_text : Screen.t -> font -> string -> int * int -> unit
(** [draw_text screen font txt (x, y)] will draw text at the specified
    location using the specified font *)
