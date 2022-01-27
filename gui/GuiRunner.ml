open Graphics
open Game
open Yojson.Basic.Util

type t = {
  keys : KeyReader.t;
  scenes : Scene.t list;
  current_scene : Scene.t;
  screen : Screen.t;
}

let init_scenes (path, file) dims =
  let json = Yojson.Basic.from_file (path ^ file) in
  let rec init_loop = function
    | [] -> []
    | h :: t ->
        let file = h |> member "scene" |> to_string in
        Scene.init path file dims :: init_loop t
  in
  init_loop (to_list json)

let init (w, h) (path, file) =
  BindFunctions.bind_funcs;
  let scenes =
    init_scenes (path, file) (float_of_int w, float_of_int h)
  in
  {
    keys = KeyReader.init ();
    scenes;
    current_scene = List.hd scenes;
    screen = Screen.setup (w, h);
  }

let rec find_scene name = function
  | [] -> None
  | h :: t ->
      if Scene.get_name h = name then Some h else find_scene name t

let update gui =
  KeyReader.update_keys gui.keys;
  let next_scene =
    match Scene.update gui.current_scene gui.keys with
    | Some s -> find_scene s gui.scenes
    | None -> None
  in
  {
    gui with
    current_scene =
      (match next_scene with
      | Some sc -> sc
      | None -> gui.current_scene);
  }

let draw gui = Scene.draw gui.current_scene gui.screen

let check_quit gui = KeyReader.check_quit gui.keys

let shutdown () = Sdl.quit ()