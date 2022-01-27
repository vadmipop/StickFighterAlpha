open Yojson.Basic.Util

type line = (float * float) * (float * float)

type letter = line array

type font = { size : float }

let make_line json =
  let pts = json |> to_list in
  match pts with
  | [ x1; y1; x2; y2 ] ->
      ((to_float x1, to_float y1), (to_float x2, to_float y2))
  | _ -> failwith "Not a line"

let rec make_letters ltrs = function
  | [] -> ()
  | (c, j) :: t ->
      let lines = j |> to_list |> List.map make_line in
      let index = Char.code (String.get c 0) in
      ltrs.(index) <- Some (Array.of_list lines);
      make_letters ltrs t

let letters =
  let json = Yojson.Basic.from_file "data/letters.json" in
  let ltrs = Array.make 256 None in
  make_letters ltrs (to_assoc json);
  ltrs

let make_font size = { size }

let scale_line size ((x1, y1), (x2, y2)) =
  ((x1 *. size, y1 *. size), (x2 *. size, y2 *. size))

let trans_line (dx, dy) ((x1, y1), (x2, y2)) =
  ((x1 +. dx, y1 +. dy), (x2 +. dx, y2 +. dy))

let int_line ((x1, y1), (x2, y2)) =
  ( (int_of_float x1, int_of_float y1),
    (int_of_float x2, int_of_float y2) )

let print_line ((x1, y1), (x2, y2)) =
  print_endline
    ((string_of_int x1 ^ ", " ^ string_of_int y1)
    ^ "--" ^ string_of_int x2 ^ ", " ^ string_of_int y2)

let draw_letter (screen : Screen.t) font c (x, y) =
  let index = Char.code c in
  match letters.(index) with
  | Some ltr ->
      for i = 0 to Array.length ltr - 1 do
        let ln =
          ltr.(i) |> scale_line font.size
          |> trans_line (x, y)
          |> int_line
        in
        Sdlrender.draw_line screen.renderer ln
      done
  | None -> ()

let draw_text (screen : Screen.t) font txt (x, y) =
  let txt = String.uppercase_ascii txt in
  let fx =
    float_of_int x
    -. (0.5 *. font.size *. float_of_int (String.length txt))
  in
  let fy = float_of_int y -. (0.6 *. font.size) in
  let drawx = ref fx in
  for i = 0 to String.length txt - 1 do
    let c = String.get txt i in
    draw_letter screen font c (!drawx, fy);
    drawx := !drawx +. font.size
  done
