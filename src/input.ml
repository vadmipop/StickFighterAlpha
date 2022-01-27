open Vector
open Figure
open Yojson.Basic.Util
open Constants
open KeyReader

type move_t = {
  command : string list;
  name : string;
  direction : Direction.dir;
  consumed : bool;
}

type t = move_t list

let rec init_bindings = function
  | [] -> []
  | h :: t ->
      {
        command = List.map to_string (to_list (member "key" h));
        name = to_string (member "move" h);
        direction =
          h |> member "direction" |> to_string |> Direction.of_string;
        consumed = h |> member "consumed" |> to_bool;
      }
      :: init_bindings t

let init json_file =
  let bindings =
    init_bindings (to_list (Yojson.Basic.from_file json_file))
  in
  bindings

(** [assign_force_main fig] assigns unit force to [fig] in the direction
    specified by user input*)
let assign_force_main bindings fig keys =
  let rec loop = function
    | [] -> fig
    | move :: t ->
        let f = loop t in
        if check_keys keys move.command then
          let fig_option =
            Figure.apply_move f move.name move.direction
          in
          match fig_option with
          | Some fig ->
              if move.consumed then remove_keys keys move.command;
              fig
          | None -> f
        else f
  in
  loop bindings
(* let fig = if Keyset.mem "A" keys then Figure.move fig
   Constants.left_force else fig in let fig = if Keyset.mem "D" keys
   then Figure.move fig Constants.right_force else fig in let fig = if
   Keyset.mem "W" keys then Figure.jump fig Constants.jump_force else
   fig in fig *)

(* if Graphics.key_pressed () then match Graphics.read_key () with | 'w'
   -> Figure.add_force fig unit_force_y | 'a' -> Figure.add_force fig
   (Vector.scale (-1.0) unit_force_x) | 's' -> Figure.add_force fig
   (Vector.scale (-1.0) unit_force_y) | 'd' -> Figure.add_force fig
   unit_force_x | _ -> Figure.add_force fig zero_force else
   Figure.add_force fig zero_force *)

let handle_input bindings keys engine =
  let player = Engine.get_figure engine "P1" in
  match player with
  | Some (Alive p) ->
      let new_fig = assign_force_main bindings p keys in
      Engine.update_figure engine new_fig
  | _ -> engine