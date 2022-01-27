open Graphics
open Yojson.Basic.Util
open JsonFunctions

type element =
  | Button of ButtonObj.t
  | Txt of TextObj.t
  | Line of LineObj.t
  | Rect of RectObj.t
  | Gme of GameObj.t

type t = {
  name : string;
  elements : (string * bool * element) array;
}

type lambda = t * element -> string option

let funcs : (string * lambda) list ref = ref []

let bind str func = funcs := (str, func) :: !funcs

let run name scene e =
  match List.assoc_opt name !funcs with
  | Some func -> func (scene, e)
  | None -> None

let rec elements_json dims path ls =
  match ls with
  | [] -> []
  | h :: t ->
      let elem_type = h |> member "type" |> to_string in
      let elem =
        match elem_type with
        | "button" -> Button (ButtonObj.init h)
        | "text" -> Txt (TextObj.init h)
        | "rect" -> Rect (RectObj.init h)
        | "line" -> Line (LineObj.init h)
        | "game" -> Gme (GameObj.init h dims)
        | _ -> failwith "Not an element type"
      in
      let name =
        member_or_default (fun j -> to_string (member "name" j)) "" h
      in
      let show =
        member_or_default (fun j -> to_bool (member "show" j)) true h
      in
      (name, show, elem) :: elements_json dims path t

let get_name scene = scene.name

let get_elem scene name =
  if name = "" then None
  else
    let el = ref None in
    for i = 0 to Array.length scene.elements - 1 do
      let namei, _, eli = scene.elements.(i) in
      if namei = name then el := Some eli
    done;
    !el

let show_elem scene name =
  if name = "" then ()
  else
    for i = 0 to Array.length scene.elements - 1 do
      let namei, showi, eli = scene.elements.(i) in
      if name = namei then scene.elements.(i) <- (namei, true, eli)
    done

let hide_elem scene name =
  if name = "" then ()
  else
    for i = 0 to Array.length scene.elements - 1 do
      let namei, showi, eli = scene.elements.(i) in
      if name = namei then scene.elements.(i) <- (namei, false, eli)
    done

let rec bind_games scene elements =
  for i = 0 to Array.length elements - 1 do
    match elements.(i) with
    | _, _, Gme game ->
        GameObj.bind game (fun game ->
            show_elem scene (GameObj.continue_button game))
    | _ -> ()
  done

let init path file dims =
  let json = Yojson.Basic.from_file (path ^ file) in
  let elements =
    json |> member "elements" |> to_list |> elements_json dims path
    |> Array.of_list
  in
  let scene = { name = json |> member "name" |> to_string; elements } in
  bind_games scene scene.elements;
  scene

let update scene keys =
  let mouse = Sdlmouse.get_state () in
  let next_scene = ref None in
  for i = 0 to Array.length scene.elements - 1 do
    match scene.elements.(i) with
    | _, true, Button b ->
        let clicked = ButtonObj.update b mouse in
        if clicked then
          next_scene := run (ButtonObj.onclick b) scene (Button b)
    | _, true, Gme g -> GameObj.update g keys
    | _ -> ()
  done;
  !next_scene

let draw scene (screen : Screen.t) =
  Screen.clear_screen screen (255, 255, 255);
  let mouse = Sdlmouse.get_state () in
  for i = 0 to Array.length scene.elements - 1 do
    let e = scene.elements.(i) in
    match e with
    | _, true, Button obj -> ButtonObj.draw obj screen mouse
    | _, true, Txt obj -> TextObj.draw obj screen
    | _, true, Rect obj -> RectObj.draw obj screen
    | _, true, Line obj -> LineObj.draw obj screen
    | _, true, Gme obj -> GameObj.draw obj screen
    | _ -> ()
  done;
  Sdlrender.render_present screen.renderer
