open Yojson.Basic.Util
open Graphics
open JsonFunctions

type t = {
  pos : int * int;
  color : int * int * int;
  text : string;
  size : float;
}

let init json =
  {
    pos = (json |> member "x" |> to_int, json |> member "y" |> to_int);
    color =
      member_or_default
        (fun j -> init_color (member "color" j))
        (0, 0, 0) json;
    text = json |> member "text" |> to_string;
    size = json |> member "text_size" |> to_float;
  }

let draw tobj (screen : Screen.t) =
  let rndr = screen.renderer in
  Sdlrender.set_draw_color rndr tobj.color 255;
  let font = Text.make_font tobj.size in
  Text.draw_text screen font tobj.text tobj.pos
