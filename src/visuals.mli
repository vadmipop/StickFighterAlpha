(** An interface between engine and screen, to control all graphics and
    rendering of the game *)

exception NoWindow

val draw_frame : Graphics.Screen.t -> Engine.t -> unit
(** Clear the previous frame and replace it with a new frame. Will throw
    an exception NoWindow if no window is currently open. *)
