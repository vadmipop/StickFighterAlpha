open Yojson.Basic.Util
open Graphics
open JsonFunctions

type t = {
  pt1 : int * int;
  pt2 : int * int;
  color : int * int * int;
}

let init json =
  {
    pt1 = (json |> member "x1" |> to_int, json |> member "y1" |> to_int);
    pt2 = (json |> member "x2" |> to_int, json |> member "y2" |> to_int);
    color =
      member_or_default
        (fun j -> init_color (member "color" j))
        (0, 0, 0) json;
  }

let draw lobj (screen : Screen.t) =
  Sdlrender.set_draw_color screen.renderer lobj.color 255;
  Sdlrender.draw_line screen.renderer (lobj.pt1, lobj.pt2)
