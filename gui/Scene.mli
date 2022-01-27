(** A scene, the basic building block of our GUI. Scenes can hold GUI
    elements, and will control all updates and drawing of those
    elements. *)

open Game

type t

type element =
  | Button of ButtonObj.t
  | Txt of TextObj.t
  | Line of LineObj.t
  | Rect of RectObj.t
  | Gme of GameObj.t

type lambda = t * element -> string option
(** A function to be called on a button click or the like. *)

val bind : string -> lambda -> unit
(** [bind name f] will bind the function [f] to [name], so that
    functions can be run just through their name. Useful for the GUI *)

val init : string -> string -> float * float -> t
(** [init path file dims] will initialize a scene from the specified
    [file], with dimensions [dims] *)

val get_name : t -> string
(** [get_name scene] is the name of [scene] *)

val get_elem : t -> string -> element option
(** [get_elem scene name] returns [Some el] if [el] is an element with
    name [name]. If no such element exists, returns [None] *)

val show_elem : t -> string -> unit
(** [show_elem scene name] will make the element with name [name]
    visible *)

val hide_elem : t -> string -> unit
(** [hide_elem scene name] will make the element with name [name]
    invisible *)

val update : t -> KeyReader.t -> string option
(** [update scene keys] will update [scene], and return [Some str] where
    [str] is the name of the next scene to transition to. Will return
    [None] if no transition should occur. *)

val draw : t -> Graphics.Screen.t -> unit
(** [draw scene screen] will render [scene] to [screen] *)