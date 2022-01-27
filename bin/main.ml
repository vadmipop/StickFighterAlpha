open Game
open Gui

type t = {
  gui : GuiRunner.t;
  frames : int;
}

let frame_rate = 30.

let frame_time = 1. /. frame_rate

(** Main game runner *)
let _ =
  let rec game_loop state =
    let start_time = Unix.gettimeofday () in
    let gui = GuiRunner.update state.gui in
    GuiRunner.draw gui;
    let end_time = Unix.gettimeofday () in
    let delta = end_time -. start_time in
    Unix.sleepf (Float.max 0. (frame_time -. delta));
    if GuiRunner.check_quit gui (* || state.frames > 100 *) then
      GuiRunner.shutdown ()
    else game_loop { gui; frames = state.frames + 1 }
  in
  let state =
    {
      gui = GuiRunner.init (600, 400) ("data/scenes/", "scenes.json");
      frames = 0;
    }
  in
  game_loop state