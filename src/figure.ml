open Yojson.Basic.Util

type t = {
  hitboxes : RigidPoly.t list;
  position : Vector.t;
  velocity : Vector.t;
  force : Vector.t;
  mass : float;
  name : string;
  on_ground : bool;
  moving : bool;
  max_speed : float * float;
  moves : Move.t list;
  current_move : Move.t;
  direction : Direction.dir;
  damage : float;
  damage_frames : int;
  color : int * int * int;
}

let init_empty_figure () =
  {
    hitboxes = [];
    position = Vector.zero;
    velocity = Vector.zero;
    force = Vector.zero;
    mass = 0.0;
    name = "empty";
    on_ground = false;
    moving = false;
    max_speed = (0.0, 0.0);
    moves = [];
    current_move =
      Move.init_from_json
        (Yojson.Basic.from_file "data/moves/empty_move.json");
    direction = Any;
    damage = 0.;
    damage_frames = 0;
    color = (0, 0, 0);
  }

(** the following functions are used for processing JSON files *)
let hbox_mem hitbox s1 = hitbox |> member s1

let hbox_mem2 hitbox s1 s2 = hitbox |> member s1 |> member s2

let get_hitboxes_json j = j |> member "hitboxes" |> to_list

let rec set_hitboxes hitboxes =
  match hitboxes with
  | [] -> []
  | h :: t ->
      RigidPoly.init_rect
        (Yojson.Basic.Util.to_float (hbox_mem h "width"))
        (Yojson.Basic.Util.to_float (hbox_mem h "height"))
        (Vector.from_json (member "center" h))
      :: set_hitboxes t

let dir_facing t = if Vector.x t.velocity >= 0.0 then true else false

let init_moves path j =
  (*if (dir_facing fig) = to_bool (member "direction" j) then*)
  let rec loop = function
    | [] -> []
    | h :: t ->
        (if to_string (member "type" h) = "file" then
         let json =
           Yojson.Basic.from_file
             (Filename.concat path (to_string (member "file" h)))
         in
         Move.init_from_json json
        else Move.init_from_json h)
        :: loop t
  in
  loop (to_list (member "moves" j))
(*else []*)

let init_color json =
  match to_list json with
  | [ r; g; b ] -> (to_int r, to_int g, to_int b)
  | _ -> failwith "Not a color"

let init_from_json path position j =
  let moves = init_moves path j in
  {
    hitboxes = set_hitboxes (get_hitboxes_json j);
    position;
    velocity = Vector.zero;
    force = Vector.zero;
    mass = 1.;
    name = j |> member "name" |> to_string;
    on_ground = false;
    moving = false;
    max_speed = (Constants.max_speed_x, Constants.max_speed_y);
    moves;
    current_move = List.nth moves 0;
    direction =
      (match to_string (member "direction" j) with
      | "left" -> Left
      | "right" -> Right
      | _ -> failwith "Invalid player direction");
    damage = 0.;
    damage_frames = 0;
    color = init_color (member "color" j);
  }

(* end of JSON processing functions *)

(* let init name w h center = { hitboxes = [ RigidPoly.init_rect w h
   Vector.zero ]; position = center; velocity = Vector.zero; force =
   Vector.zero; mass = 1.; name; on_ground = false; moving = false;
   max_speed = (Constants.max_speed_x, Constants.max_speed_y); } *)

let on_ground fig = fig.on_ground

let rec apply_delay name = function
  | [] -> []
  | h :: t ->
      (if Move.get_name h = name then Move.not_ready h else h)
      :: apply_delay name t

let advance_move fig =
  let nxt = Move.advance fig.current_move in
  match nxt with
  | Some mv -> { fig with current_move = mv }
  | None ->
      {
        fig with
        current_move = List.nth fig.moves 0;
        moves = apply_delay (Move.get_name fig.current_move) fig.moves;
      }

let location fig = if fig.on_ground then "ground" else "air"

let advance_delays fig =
  let rec adv_loop = function
    | [] -> []
    | h :: t -> Move.advance_reset h (location fig) :: adv_loop t
  in
  { fig with moves = adv_loop fig.moves }

let overlap_ter fig hitbox ter =
  let collidables = Terrain.get_polys ter in
  let rec overlap_adjust acc = function
    | [] -> acc
    | h :: t ->
        let overlap =
          RigidPoly.overlap_dir h (RigidPoly.move fig.position hitbox)
        in
        overlap_adjust (Vector.add overlap acc) t
  in
  overlap_adjust Vector.zero collidables

let check_ground fig hitbox ter =
  let collidables = Terrain.get_polys ter in
  let min_dist =
    List.fold_left
      (fun min_d box ->
        let dist, norm =
          RigidPoly.nearest box (RigidPoly.move fig.position hitbox)
        in
        if Vector.dot norm Constants.up > 0.5 then Float.min dist min_d
        else min_d)
      Constants.touching_ground collidables
  in
  min_dist < Constants.touching_ground

let update fig ter =
  let accel = Vector.scale (1. /. fig.mass) fig.force in
  let new_vel =
    fig.velocity
    |> Vector.scale Constants.air_friction
    |> Vector.add accel
    |> Vector.constrain_xy fig.max_speed
  in
  let new_vel =
    if fig.on_ground && not fig.moving then
      Vector.scale Constants.ground_friction new_vel
    else new_vel
  in
  let new_fig =
    {
      fig with
      position = Vector.add fig.position new_vel;
      velocity = new_vel;
      force = Vector.zero;
      moving = false;
      damage_frames = max 0 (fig.damage_frames - 1);
    }
  in
  let rec fix_hitbox_overlap acc = function
    | [] -> acc
    | h :: t ->
        fix_hitbox_overlap
          (Vector.add acc (overlap_ter new_fig h ter))
          t
  in
  let adjust = fix_hitbox_overlap Vector.zero new_fig.hitboxes in
  let rec check_all_ground = function
    | [] -> false
    | h :: t -> check_ground new_fig h ter || check_all_ground t
  in
  let on_ground = check_all_ground new_fig.hitboxes in
  let new_fig =
    if Vector.equal adjust Vector.zero then { new_fig with on_ground }
    else
      let unit_adjust = Vector.unitize adjust in
      let bad_vel =
        Vector.scale
          (Float.min 0. (Vector.dot new_vel unit_adjust))
          unit_adjust
      in
      let fixed_vel =
        Vector.sub new_vel
          (Vector.scale (1. +. Constants.bounce_dampening) bad_vel)
      in
      {
        new_fig with
        position = Vector.add new_fig.position adjust;
        velocity = fixed_vel;
        on_ground;
      }
  in
  advance_delays new_fig

let get_force fig = fig.force

let add_force fig f = { fig with force = Vector.add fig.force f }

let add_force_controlled fig f bound =
  let vel = Vector.length fig.velocity in
  let unitv = Vector.unitize fig.velocity in
  let f =
    if vel < bound then
      let extended = Vector.dot f unitv in
      if extended +. vel > bound then
        let sub = Vector.scale (extended +. vel -. bound) unitv in
        Vector.sub f sub
      else f
    else if Vector.dot unitv f > 0. then
      let norm = Vector.norm fig.velocity in
      Vector.scale (Vector.dot norm f) norm
    else f
  in
  { fig with force = Vector.add fig.force f }

let clear_vel fig (add_x, add_y) =
  let x = if add_x then Vector.x fig.velocity else 0. in
  let y = if add_y then Vector.y fig.velocity else 0. in
  { fig with velocity = Vector.init x y }

let add_damage fig dmg =
  if fig.damage_frames <= 0 then
    {
      fig with
      damage = fig.damage +. dmg;
      damage_frames =
        (if dmg <> 0. then Constants.damage_frames
        else fig.damage_frames);
    }
  else fig

let get_damage fig = fig.damage

let jump fig f =
  if fig.on_ground then
    { fig with force = Vector.add fig.force f; moving = true }
  else fig

let apply_move fig name direction =
  if
    (not (Move.get_changeable fig.current_move))
    || fig.damage_frames <> 0
  then None
  else
    let rec loop = function
      | [] -> None
      | h :: t -> if Move.get_name h = name then Some h else loop t
    in
    match loop fig.moves with
    | Some mv ->
        if not (Move.ready mv) then None
        else
          let mv =
            if Move.get_name fig.current_move = name then
              fig.current_move
            else mv
          in
          let valid_location =
            match Move.get_where mv with
            | "any" -> true
            | "ground" -> fig.on_ground
            | "air" -> not fig.on_ground
            | _ -> failwith "Invalid move location"
          in
          if valid_location then
            let d =
              if direction = Direction.Any then fig.direction
              else direction
            in
            let mv = Move.of_direction d mv in
            let f = Move.get_force mv in
            let additive = Move.additive_force mv in
            let moving = if f = Vector.zero then false else true in
            let fig =
              if Move.get_controlled mv then
                clear_vel
                  (add_force_controlled
                     {
                       fig with
                       current_move = mv;
                       moving;
                       direction = d;
                     }
                     f Constants.control_speed)
                  additive
              else
                clear_vel
                  (add_force
                     {
                       fig with
                       current_move = mv;
                       moving;
                       direction = d;
                     }
                     f)
                  additive
            in
            Some fig
          else None
    | None -> None

let add_grav fig =
  if fig.on_ground then fig
  else
    {
      fig with
      force = Vector.add fig.force Constants.grav;
      moving = true;
    }

let speed fig = Vector.length fig.velocity

let bound_speed fig low high =
  { fig with velocity = Vector.constrain_length low high fig.velocity }

let get_hitboxes fig =
  List.map (RigidPoly.move fig.position) fig.hitboxes

let get_name fig = fig.name

let get_pos fig = fig.position

let get_move fig = fig.current_move

let get_lines fig =
  let frame = Move.get_animation fig.current_move in
  List.fold_left
    (fun acc (s1, s2) ->
      let p1 = Animation.get_point s1 frame in
      let p2 = Animation.get_point s2 frame in
      match (p1, p2) with
      | Some v1, Some v2 ->
          (v1 |> Vector.add fig.position, v2 |> Vector.add fig.position)
          :: acc
      | _ -> acc)
    [] Constants.figure_lines

let get_color fig = fig.color