(** Controls the screen / window, including functions for opening and
    quitting the game window. *)

type t = {
  width : int;
  height : int;
  window : Sdlwindow.t;
  renderer : Sdlrender.t;
  surface : Sdlsurface.t;
}

(** [flip screen pt] will reflect the [pt] around the horizontal axis of
    the screen *)
let flip screen (x, y) = (x, screen.height - y)

(** [clear_events ()] will clear / discard all remaining events to be
    polled *)
let rec clear_events () =
  match Sdlevent.poll_event () with
  | Some e -> clear_events ()
  | None -> ()

(** [clear_screen screen color] will wipe the screen with the specified
    color *)
let clear_screen screen (r, g, b) =
  Sdlrender.set_draw_color3 screen.renderer 255 255 255 255;
  Sdlrender.clear screen.renderer

(** [setup (width, height)] will create a new window with the specified
    dimensions *)
let setup (width, height) =
  Sdl.init [ `VIDEO ];
  let window, renderer =
    Sdlrender.create_window_and_renderer ~width ~height ~flags:[]
  in
  clear_events ();
  Sdlrender.set_draw_blend_mode renderer SdlblendMode.Blend;
  {
    width;
    height;
    window;
    renderer;
    surface = Sdlwindow.get_surface window;
  }

(** [shutdown ()] will immediately close the window and end the program *)
let shutdown _ = Sdl.quit ()
