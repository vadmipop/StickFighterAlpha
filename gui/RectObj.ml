open Yojson.Basic.Util
open Graphics
open JsonFunctions

type t = {
  mutable rect : Sdlrect.t;
  color : int * int * int;
}

let init json =
  {
    rect =
      {
        x = json |> member "x" |> to_int;
        y = json |> member "y" |> to_int;
        w = json |> member "width" |> to_int;
        h = json |> member "height" |> to_int;
      };
    color =
      member_or_default
        (fun j -> init_color (member "color" j))
        (0, 0, 0) json;
  }

let moveto robj (x, y) =
  let new_rect = { robj.rect with x; y } in
  robj.rect <- new_rect

let draw robj (screen : Screen.t) =
  Sdlrender.set_draw_color screen.renderer robj.color 255;
  Sdlrender.draw_rect screen.renderer robj.rect
