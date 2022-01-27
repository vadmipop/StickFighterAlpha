(** Runs the entire GUI, and manages switching between scenes *)

type t

val init : int * int -> string * string -> t
(** [init (width, height) (path, file)] will initialize a gui with the
    specified width and height, with information from the file*)

val update : t -> t
(** [update gui] will perform any updates on the gui *)

val draw : t -> unit
(** [draw gui] will render the gui to the current window *)

val check_quit : t -> bool
(** [check_quit gui] will check if the gui has been quit (the escape key
    has been pressed) *)

val shutdown : unit -> unit
(** [shutdown ()] will immediately shutdown the gui and close all
    windows *)
