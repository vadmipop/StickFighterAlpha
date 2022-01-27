open Yojson.Basic.Util
open Graphics
open JsonFunctions
open Game

type t = {
  bindings : Input.t;
  file : string;
  dims : float * float;
  mutable eng : Engine.t option;
  mutable win_timer : int;
  mutable winner : Figure.t option;
  continue_button : string;
  mutable on_win : bool * (t -> unit);
}

let init json dims =
  let file = json |> member "file" |> to_string in
  {
    eng = None;
    file;
    dims;
    bindings = Input.init "data/controls.json";
    win_timer = -1;
    winner = None;
    continue_button = json |> member "continue_button" |> to_string;
    on_win = (false, fun game -> ());
  }

let reset game =
  game.eng <- None;
  game.win_timer <- -1;
  game.winner <- None

let init_eng game = game.eng <- Some (Engine.init game.dims)

let bind game f = game.on_win <- (false, f)

let continue_button game = game.continue_button

let update game keys =
  if game.eng = None then init_eng game;
  match game.eng with
  | Some eng ->
      if game.win_timer > 0 then game.win_timer <- game.win_timer - 1;
      let eng = Cpu.update eng in
      let eng = Engine.update eng in
      let eng = Input.handle_input game.bindings keys eng in
      game.eng <- Some eng;
      let winner = Engine.check_winner eng in
      if winner <> None && game.win_timer < 0 then (
        game.win_timer <- Constants.win_timer;
        game.winner <- winner);
      if game.win_timer == 0 && not (fst game.on_win) then
        (snd game.on_win) game
  | None -> failwith "Impossible"

let draw game (screen : Screen.t) =
  match game.eng with
  | Some eng ->
      Visuals.draw_frame screen eng;
      if game.win_timer >= 0 then begin
        let rndr = screen.renderer in
        Sdlrender.set_draw_color rndr (0, 0, 0)
          (min 150 (10 * (Constants.win_timer - game.win_timer)));
        Sdlrender.fill_rect rndr
          (Sdlrect.make (0, 0) (screen.width, screen.height));
        let size =
          float_of_int (game.win_timer * game.win_timer / 5) +. 64.
        in
        let fnt = Text.make_font size in
        Sdlrender.set_draw_color rndr (255, 255, 255) 255;
        let x = screen.width / 2 in
        let y = screen.height * 2 / 5 in
        Text.draw_text screen fnt " GAME!" (x, y);
        let w = int_of_float (6.4 *. size) in
        let h = int_of_float (1.6 *. size) in
        Sdlrender.draw_rect rndr
          (Sdlrect.make (x - (w / 2), y - (h / 2)) (w, h));

        if game.win_timer == 0 then
          let winner =
            match game.winner with
            | Some f -> f
            | None -> failwith "Impossible"
          in
          let fnt = Text.make_font 32. in
          let name = Figure.get_name winner in
          let y = y + int_of_float (size *. 0.6) + 45 in
          Text.draw_text screen fnt ("Winner: " ^ name) (x, y)
      end
  | None -> ()
