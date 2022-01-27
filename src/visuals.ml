open Graphics
open Graphics.Screen

exception NoWindow

let draw_poly screen poly shift =
  let trans = Vector.add (RigidPoly.center poly) in
  let pts = Array.of_list (List.map trans (RigidPoly.vertices poly)) in
  for i = 0 to Array.length pts - 2 do
    let pt = flip screen (Vector.to_ints (Vector.sub pts.(i) shift)) in
    let next_pt =
      flip screen (Vector.to_ints (Vector.sub pts.(i + 1) shift))
    in
    Sdlrender.draw_line2 screen.renderer ~p1:pt ~p2:next_pt
  done;
  let pt = flip screen (Vector.to_ints (Vector.sub pts.(0) shift)) in
  let next_pt =
    flip screen
      (Vector.to_ints (Vector.sub pts.(Array.length pts - 1) shift))
  in
  Sdlrender.draw_line2 screen.renderer ~p1:pt ~p2:next_pt

let fill_poly screen poly shift =
  let trans = Vector.add (RigidPoly.center poly) in
  let pts = Array.of_list (List.map trans (RigidPoly.vertices poly)) in
  for i = 0 to Array.length pts - 2 do
    let pt = flip screen (Vector.to_ints (Vector.sub pts.(i) shift)) in
    let next_pt =
      flip screen (Vector.to_ints (Vector.sub pts.(i + 1) shift))
    in
    Sdlrender.draw_line2 screen.renderer ~p1:pt ~p2:next_pt
  done;
  let pt = flip screen (Vector.to_ints (Vector.sub pts.(0) shift)) in
  let next_pt =
    flip screen
      (Vector.to_ints (Vector.sub pts.(Array.length pts - 1) shift))
  in
  Sdlrender.draw_line2 screen.renderer ~p1:pt ~p2:next_pt

let draw_fig screen fig shift =
  let color = Figure.get_color fig in
  Sdlrender.set_draw_color screen.renderer color 255;
  let lines = Array.of_list (Figure.get_lines fig) in
  for i = 0 to Array.length lines - 1 do
    let p1, p2 = lines.(i) in
    let p1 = flip screen (Vector.to_ints (Vector.sub p1 shift)) in
    let p2 = flip screen (Vector.to_ints (Vector.sub p2 shift)) in
    Sdlrender.draw_line screen.renderer (p1, p2)
  done;
  if Constants.debug then (
    let move = Figure.get_move fig in
    let pos = Figure.get_pos fig in
    Sdlrender.set_draw_color3 screen.renderer 0 0 255 255;
    for i = 0 to List.length (Move.get_hurtboxes move) - 1 do
      let hb = List.nth (Move.get_hurtboxes move) i in
      draw_poly screen (RigidPoly.move pos hb) shift
    done;
    Sdlrender.set_draw_color3 screen.renderer 0 255 0 255;
    for i = 0 to List.length (Move.get_defense_boxes move) - 1 do
      let hb = List.nth (Move.get_defense_boxes move) i in
      draw_poly screen (RigidPoly.move pos hb) shift
    done;
    Sdlrender.set_draw_color3 screen.renderer 255 0 0 255;
    for i = 0 to List.length (Move.get_neutral_boxes move) - 1 do
      let hb = List.nth (Move.get_neutral_boxes move) i in
      draw_poly screen (RigidPoly.move pos hb) shift
    done)

let round1 n = Float.(n *. 10.0 |> round |> fun x -> x /. 10.0)

let flt_str n =
  let n = round1 n in
  string_of_float n ^ if mod_float n 1.0 = 0.0 then "0%" else "%"

let draw_health screen rect fig =
  let rndr = screen.renderer in
  let dmg = Figure.get_damage fig in
  Sdlrender.set_draw_color rndr (200, 200, 200) 150;
  Sdlrender.fill_rect rndr rect;
  Sdlrender.set_draw_color rndr (0, 0, 0) 255;
  Sdlrender.draw_rect rndr rect;
  let cx = rect.x + (rect.w / 2) in
  let txt = Figure.get_name fig in
  let fnt =
    Text.make_font
      (Float.min
         (float_of_int rect.h *. 0.15)
         (float_of_int rect.w
         /. (float_of_int (String.length txt) +. 1.)))
  in
  Text.draw_text screen fnt txt (cx, rect.y + (rect.h / 5));
  let cy = rect.y + (rect.h * 2 / 3) in
  let r = int_of_float (Float.min 250. (2.5 *. dmg)) in
  let txt = flt_str dmg in
  let fnt =
    Text.make_font
      (Float.min
         (float_of_int rect.h *. 0.4)
         (float_of_int rect.w
         /. (float_of_int (String.length txt) +. 1.)))
  in
  Sdlrender.set_draw_color rndr (r, 0, 0) 255;
  Text.draw_text screen fnt txt (cx, cy)

let draw_dead screen rect fig =
  let rndr = screen.renderer in
  Sdlrender.set_draw_color rndr (255, 150, 150) 150;
  Sdlrender.fill_rect rndr rect;
  Sdlrender.set_draw_color rndr (0, 0, 0) 255;
  Sdlrender.draw_rect rndr rect;
  let cx = rect.x + (rect.w / 2) in
  let txt = Figure.get_name fig in
  let fnt =
    Text.make_font
      (Float.min
         (float_of_int rect.h *. 0.15)
         (float_of_int rect.w
         /. (float_of_int (String.length txt) +. 1.)))
  in
  Text.draw_text screen fnt txt (cx, rect.y + (rect.h / 5));
  let cy = rect.y + (rect.h * 2 / 3) in
  let txt = "DEAD" in
  let fnt =
    Text.make_font
      (Float.min
         (float_of_int rect.h *. 0.4)
         (float_of_int rect.w
         /. (float_of_int (String.length txt) +. 1.)))
  in
  Sdlrender.set_draw_color rndr (0, 0, 0) 255;
  Text.draw_text screen fnt txt (cx, cy)

let screen_dims screen =
  Vector.init (Float.of_int screen.width) (Float.of_int screen.height)

let draw_moves screen move shift =
  Sdlrender.set_draw_color3 screen.renderer 0 0 255 255;
  let hubs = Move.get_hurtboxes move in
  for i = 0 to List.length hubs - 1 do
    draw_poly screen (List.nth hubs i) shift
  done

let draw_frame screen eng =
  Sdlrender.set_draw_color3 screen.renderer 0 0 0 255;
  let shift =
    Camera.coords (Engine.get_camera eng) (screen_dims screen)
  in
  let polys =
    Terrain.get_polys (Engine.get_terrain eng) |> Array.of_list
  in
  for i = 0 to Array.length polys - 1 do
    draw_poly screen polys.(i) shift
  done;
  let figures = Array.of_list (Engine.get_figures eng) in
  let num_figs = Array.length figures in
  let y = screen.height * 3 / 4 in
  let height = screen.height / 5 in
  for i = 0 to num_figs - 1 do
    let cx = (i + 1) * screen.width / (num_figs + 1) in
    let width = screen.width / (num_figs + 2) in
    let rect = Sdlrect.make (cx - (width / 2), y) (width, height) in
    match figures.(i) with
    | Alive fig ->
        draw_fig screen fig shift;
        draw_health screen rect fig
    | Dead fig -> draw_dead screen rect fig
  done;
  ()