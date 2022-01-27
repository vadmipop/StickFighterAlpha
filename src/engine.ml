open Terrain
open Vector
open Figure
open Yojson.Basic.Util

(** The following code is for processing JSONs*)

type player =
  | Alive of Figure.t
  | Dead of Figure.t

type platform = {
  name : string;
  poly : RigidPoly.t;
}

type t = {
  terrain : Terrain.t;
  figures : player list;
  camera : Camera.t;
  (* cpu : Cpu.t list; *)
  death : RigidPoly.t;
}

let player_file = ref "data/red_player.json"

let level_file = ref "data/level1.json"

let rect_from_json json =
  let height = json |> member "height" |> to_float in
  let width = json |> member "width" |> to_float in
  let center = json |> member "center" |> Vector.from_json in
  RigidPoly.init_rect width height center

let poly_from_json json =
  let points =
    json |> member "points" |> to_list |> List.map Vector.from_json
  in
  let center = json |> member "center" |> Vector.from_json in
  RigidPoly.init points center

let platform_from_json json =
  {
    name = json |> member "name" |> to_string;
    poly =
      (let kind = json |> member "type" |> to_string in
       if kind = "rect" then rect_from_json json
       else poly_from_json json);
  }

let poly_of_platform platform = platform.poly

let rec figures_from_json path =
  List.map (fun (json, pos) -> Figure.init_from_json path pos json)

let make_figure path fig =
  let typ = fig |> member "type" |> to_string in
  let fig_json =
    match typ with
    | "file" ->
        fig |> member "file" |> to_string |> Filename.concat path
        |> Yojson.Basic.from_file
    | "custom" -> Yojson.Basic.from_file !player_file
    | _ -> failwith "Not a valid player type"
  in
  let fig_pos = fig |> member "position" |> Vector.from_json in
  (fig_json, fig_pos)

let from_file file dims =
  let path = Filename.dirname file in
  let json = Yojson.Basic.from_file file in
  let death_json = member "death" json in
  let death_height = death_json |> member "height" |> to_float in
  let death_width = death_json |> member "width" |> to_float in
  let death_center =
    death_json |> member "center" |> Vector.from_json
  in
  let figs_list =
    List.map (make_figure path) (json |> member "figures" |> to_list)
  in
  let figs_json = figures_from_json path figs_list in
  {
    terrain =
      (let polys =
         json |> member "platforms" |> to_list
         |> List.map platform_from_json
         |> List.map poly_of_platform
       in
       Terrain.init polys);
    figures = List.map (fun f -> Alive f) figs_json;
    camera =
      Camera.init Vector.zero dims
        ( Vector.x death_center -. (death_width /. 2.),
          Vector.x death_center +. (death_width /. 2.) )
        ( Vector.y death_center -. (death_height /. 2.),
          Vector.y death_center +. (death_height /. 2.) )
      (* cpu = List.map Cpu.init (List.filter Cpu.is_cpu figs_json); *);
    death = RigidPoly.init_rect death_width death_height death_center;
  }

let init dims = from_file !level_file dims

let get_figures eng = eng.figures

let rec get_figure eng name =
  let rec find_by_name = function
    | [] -> None
    | h :: t -> (
        match h with
        | Alive f
        | Dead f ->
            if Figure.get_name f = name then Some h else find_by_name t)
  in
  find_by_name eng.figures

let check_attacks fig1 fig2 =
  Move.get_effect
    (Figure.get_move fig1, Figure.get_pos fig1)
    (Figure.get_move fig2, Figure.get_pos fig2)

let rec update_dead eng = function
  | [] -> []
  | Dead h :: t -> Dead h :: update_dead eng t
  | Alive h :: t ->
      let in_stage = RigidPoly.point_in eng.death (Figure.get_pos h) in
      (if in_stage then Alive h else Dead h) :: update_dead eng t

let rec update_figures eng = function
  | [] -> []
  | Alive h :: t ->
      (let rec loop = function
         | [] -> (0., Vector.zero, Vector.zero)
         | Alive h2 :: t2 ->
             let dmg, kb, pen =
               if Figure.get_name h = Figure.get_name h2 then
                 (0., Vector.zero, Vector.zero)
               else check_attacks h2 h
             in
             let dmg2, kb2, pen2 = loop t2 in
             (dmg +. dmg2, Vector.add kb kb2, Vector.add pen pen2)
         | Dead h2 :: t2 -> loop t2
       in
       let dmg, kb, pen = loop eng.figures in
       let kb =
         Vector.scale (Constants.damage_fn (Figure.get_damage h)) kb
       in
       let fig = Figure.add_force h kb in
       let fig = Figure.add_damage fig dmg in
       let fig = Figure.advance_move fig in
       let fig = Figure.update (Figure.add_grav fig) eng.terrain in
       Alive fig)
      :: update_figures eng t
  | Dead h :: t -> Dead h :: update_figures eng t

let update eng =
  let cam_target =
    match get_figure eng "P1" with
    | None -> Vector.zero
    | Some (Alive fig)
    | Some (Dead fig) ->
        Figure.get_pos fig
  in
  {
    eng with
    figures = update_dead eng (update_figures eng eng.figures);
    camera = Camera.update eng.camera cam_target;
  }

let get_terrain eng = eng.terrain

let get_camera eng = eng.camera

let update_figures eng figures = { eng with figures }

let update_figure eng fig =
  let rec loop = function
    | [] -> []
    | Alive h :: t ->
        (if Figure.get_name h = Figure.get_name fig then Alive fig
        else Alive h)
        :: loop t
    | Dead h :: t -> Dead h :: loop t
  in
  update_figures eng (loop eng.figures)

let rec alive = function
  | [] -> []
  | Alive f :: t -> f :: alive t
  | Dead f :: t -> alive t

let check_winner eng =
  match alive eng.figures with
  | [ f ] -> Some f
  | _ -> None