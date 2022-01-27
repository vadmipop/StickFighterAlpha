(** A module for binding functions to scenes. These functions are later
    access by string names as specified in their json. *)

open Game

(** start the game *)
let play_game (scene, el) = Some "game"

(** start the help screen*)
let goto_help (scene, el) = Some "help"

(** finish the game, go to the start screen *)
let goto_start (scene, el) =
  let game_el = Scene.get_elem scene "game" in
  (match game_el with
  | Some (Gme game) -> GameObj.reset game
  | _ -> ());
  Scene.hide_elem scene "continue_button";
  Some "start"

(** start the level select screen *)
let goto_select (scene, el) = Some "level_select"

(** select the first stage (level select screen) *)
let select_stage1 (scene, el) =
  (match Scene.get_elem scene "select_rect" with
  | Some (Rect r) -> RectObj.moveto r (75, 125)
  | _ -> ());
  Engine.level_file := "data/level1.json";
  None

(** select the second stage (level select screen) *)
let select_stage2 (scene, el) =
  (match Scene.get_elem scene "select_rect" with
  | Some (Rect r) -> RectObj.moveto r (230, 125)
  | _ -> ());
  Engine.level_file := "data/level2.json";
  None

(** select the third stage (level select screen) *)
let select_stage3 (scene, el) =
  (match Scene.get_elem scene "select_rect" with
  | Some (Rect r) -> RectObj.moveto r (385, 125)
  | _ -> ());
  Engine.level_file := "data/level3.json";
  None

(** select the red player (level select screen) *)
let select_red (scene, el) =
  (match Scene.get_elem scene "select_color_rect" with
  | Some (Rect r) -> RectObj.moveto r (75, 205)
  | _ -> ());
  Engine.player_file := "data/red_player.json";
  None

(** select the blue player (level select screen) *)
let select_blue (scene, el) =
  (match Scene.get_elem scene "select_color_rect" with
  | Some (Rect r) -> RectObj.moveto r (230, 205)
  | _ -> ());
  Engine.player_file := "data/blue_player.json";
  None

(** select the green player (level select screen) *)
let select_green (scene, el) =
  (match Scene.get_elem scene "select_color_rect" with
  | Some (Rect r) -> RectObj.moveto r (385, 205)
  | _ -> ());
  Engine.player_file := "data/green_player.json";
  None

(** bind all the above functions *)
let bind_funcs =
  Scene.bind "play_game" play_game;
  Scene.bind "goto_help" goto_help;
  Scene.bind "goto_start" goto_start;
  Scene.bind "goto_select" goto_select;
  Scene.bind "select_stage1" select_stage1;
  Scene.bind "select_stage2" select_stage2;
  Scene.bind "select_stage3" select_stage3;
  Scene.bind "select_red" select_red;
  Scene.bind "select_blue" select_blue;
  Scene.bind "select_green" select_green
