open Yojson.Basic.Util
open Graphics
open JsonFunctions

type t = {
  rect : Sdlrect.t;
  color : int * int * int;
  color_hover : int * int * int;
  text : string;
  text_size : float;
  text_color : int * int * int;
  text_color_hover : int * int * int;
  onclick : string;
  mutable pressed : bool;
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
        (255, 255, 255) json;
    color_hover =
      member_or_default
        (fun j -> init_color (member "color_hover" j))
        (200, 200, 200) json;
    text = json |> member "text" |> to_string;
    text_size = json |> member "text_size" |> to_float;
    text_color =
      member_or_default
        (fun j -> init_color (member "text_color" j))
        (0, 0, 0) json;
    text_color_hover =
      member_or_default
        (fun j -> init_color (member "text_color_hover" j))
        (0, 0, 0) json;
    onclick = json |> member "onclick" |> to_string;
    pressed = false;
  }

let onclick b = b.onclick

let update b (mouse, bs) =
  if not (Sdlrect.point_in_rect mouse b.rect) then false
  else
    match (b.pressed, List.mem Sdlmouse.Button_Left bs) with
    | true, true
    | false, false ->
        false
    | false, true ->
        b.pressed <- true;
        false
    | true, false ->
        b.pressed <- false;
        true

let center b = (b.rect.x + (b.rect.w / 2), b.rect.y + (b.rect.h / 2))

let draw b (screen : Screen.t) (mouse, bs) =
  let rndr = screen.renderer in
  if Sdlrect.point_in_rect mouse b.rect then
    Sdlrender.set_draw_color rndr b.color_hover 255
  else Sdlrender.set_draw_color rndr b.color 255;
  Sdlrender.fill_rect rndr b.rect;
  Sdlrender.set_draw_color rndr (0, 0, 0) 255;
  Sdlrender.draw_rect rndr b.rect;
  let font = Text.make_font b.text_size in
  if Sdlrect.point_in_rect mouse b.rect then
    Sdlrender.set_draw_color rndr b.text_color_hover 255
  else Sdlrender.set_draw_color rndr b.text_color 255;
  Text.draw_text screen font b.text (center b)
