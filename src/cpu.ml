type substate =
  | Left
  | Right
  | Up
  | Down

type state =
  | Chase of substate
  | Attack of substate
  | ChaseAir of substate
  | AttackAir of substate
  | Inactive

type t = { figure : Figure.t }

let determine_substate (cpu_pos : Vector.t) (player_pos : Vector.t) =
  let diff_vec =
    Vector.unitize (Vector.scale ~-.1. (Vector.sub cpu_pos player_pos))
  in
  let max_dir =
    if Float.abs (Vector.x diff_vec) > Float.abs (Vector.y diff_vec)
    then if Vector.x diff_vec < 0. then `Left else `Right
    else if Vector.y diff_vec < 0. then `Down
    else `Up
  in
  match max_dir with
  | `Left -> Left
  | `Right -> Right
  | `Down -> Down
  | `Up -> Up

let determine_state (cpu : t) (eng : Engine.t) =
  let player = Engine.get_figure eng "P1" in
  let player_pos =
    match player with
    | Some (Alive fig) -> Some (Figure.get_pos fig)
    | _ -> None
  in
  match player_pos with
  | None -> Inactive
  | Some vect -> (
      let cpu_pos = Figure.get_pos cpu.figure in
      let separation =
        Vector.length (Vector.sub (Figure.get_pos cpu.figure) vect)
      in
      let on_ground = Figure.on_ground cpu.figure in
      let substate = determine_substate cpu_pos vect in
      match separation with
      | d when d < 20.0 -> (
          match on_ground with
          | true -> (
              match substate with
              | Left -> Attack Left
              | Right -> Attack Right
              | Down -> Attack Down
              | Up -> Attack Up)
          | false -> (
              match substate with
              | Left -> AttackAir Left
              | Right -> AttackAir Right
              | Down -> AttackAir Down
              | Up -> AttackAir Up))
      | _ -> (
          match on_ground with
          | true -> (
              match substate with
              | Left -> Chase Left
              | Right -> Chase Right
              | Down -> Chase Down
              | Up -> Chase Up)
          | false -> (
              match substate with
              | Left -> ChaseAir Left
              | Right -> ChaseAir Right
              | Down -> ChaseAir Down
              | Up -> ChaseAir Up)))

let cpu_figure_with_move (cpu : t) (eng : Engine.t) =
  match determine_state cpu eng with
  | Attack Left -> Figure.apply_move cpu.figure "punch" Left
  | Attack Right -> Figure.apply_move cpu.figure "punch" Right
  | Attack Up -> Figure.apply_move cpu.figure "punch" Any
  | Attack Down -> Figure.apply_move cpu.figure "run" Any
  | AttackAir Left -> Figure.apply_move cpu.figure "nair" Left
  | AttackAir Right -> Figure.apply_move cpu.figure "nair" Right
  | AttackAir Up -> Figure.apply_move cpu.figure "uppercut" Any
  | AttackAir Down -> Figure.apply_move cpu.figure "down slam" Any
  | Chase Left -> Figure.apply_move cpu.figure "run" Left
  | Chase Right -> Figure.apply_move cpu.figure "run" Right
  | Chase Up -> Figure.apply_move cpu.figure "jump" Any
  | Chase Down -> Figure.apply_move cpu.figure "run" Any
  | ChaseAir Left -> Figure.apply_move cpu.figure "move_air" Left
  | ChaseAir Right -> Figure.apply_move cpu.figure "move_air" Right
  | ChaseAir Up -> Figure.apply_move cpu.figure "double_jump" Any
  | ChaseAir Down -> Figure.apply_move cpu.figure "move_air" Any
  | Inactive -> Some cpu.figure

(* type state = { name : string; action : t -> Engine.t -> t; }

   type fsm = { states : state list; transitions : (state -> state)
   list; currentState : state; } *)

(* let chase_action (cpu : t) (eng : Engine.t) = let player =
   Engine.get_figure eng "P1" in (* change if we rename the player/ add
   more players*) let player_pos = match player with | Some (Alive fig)
   -> Some (Figure.get_pos fig) | _ -> None in let cpu_force_dir = match
   player_pos with | None -> Vector.zero | Some vect -> Vector.unitize
   (Vector.scale ~-.1. (Vector.sub (Figure.get_pos cpu.figure) vect)) in
   let max_dir = if Float.abs (Vector.x cpu_force_dir) > Float.abs
   (Vector.y cpu_force_dir) then 0 else 1 in if Figure.on_ground
   cpu.figure then if max_dir = 0 then if Vector.x cpu_force_dir <= 0.
   then { figure = (match Figure.apply_move cpu.figure "move_left" Left
   with | Some figure -> figure | None -> Figure.init_empty_figure ());
   } else { figure = (match Figure.apply_move cpu.figure "move_left"
   Right with | Some figure -> figure | None -> Figure.init_empty_figure
   ()); } else if Vector.y cpu_force_dir > 0. then { figure = (match
   Figure.apply_move cpu.figure "jump" Any with | Some figure -> figure
   | None -> Figure.init_empty_figure ()); } else cpu else if max_dir =
   0 then if Vector.x cpu_force_dir <= 0. then { figure = (match
   Figure.apply_move cpu.figure "left_air" Left with | Some figure ->
   figure | None -> Figure.init_empty_figure ()); } else { figure =
   (match Figure.apply_move cpu.figure "right_air" Right with | Some
   figure -> figure | None -> Figure.init_empty_figure ()); } else
   cpu *)

(* let (chase : state) = { name = "chase"; action = chase_action } *)

(* let attack_action (cpu : t) (eng : Engine.t) = let p_figure =
   Engine.get_figure eng "P1" in let player_figure = match p_figure with
   | Some p -> p | None -> Figure.init_empty_figure () in let
   player_move = Figure.get_move player_figure in let player_nboxes =
   Move.get_neutral_boxes player_move in let player_dboxes =
   Move.get_defense_boxes player_move in let player_location =
   Figure.get_pos player_figure in let abs_nboxes = List.map (fun nb ->
   RigidPoly.move player_location nb) player_nboxes in let abs_dboxes =
   List.map (fun db -> RigidPoly.move player_location db) player_dboxes
   in let abs_ncenter = RigidPoly.average_center abs_nboxes in let
   abs_dcenter = RigidPoly.average_center abs_dboxes *)

let get_name cpu = Figure.get_name cpu

let init fig = { figure = fig }

(* (Vector.y cpu_force_dir > 0.) then {figure = Figure.jump cpu.figure
   Constants.jump_force} else {figure = Figure.add_force cpu.figure } *)

let is_cpu fig = if Figure.get_name fig = "CPU" then true else false

let update (eng : Engine.t) =
  let cpu_player = Engine.get_figure eng "CPU" in
  match cpu_player with
  | None -> eng
  | Some player -> (
      match player with
      | Engine.Alive fig
      | Engine.Dead fig -> (
          let cpu = { figure = fig } in
          let fig_with_move = cpu_figure_with_move cpu eng in
          match fig_with_move with
          | None -> eng
          | Some fig -> Engine.update_figure eng fig))

(* let update (eng : Engine.t) = let player_list = Engine.get_figures
   eng in let cpu_list = List.filter (fun player -> match player with |
   Engine.Alive fig | Engine.Dead fig -> ( match is_cpu fig with | true
   -> true | false -> false)) player_list in match cpu_list with | [ fig
   ] -> ( match fig with | Engine.Alive fig | Engine.Dead fig -> ( let
   cpu = { figure = fig } in let fig_with_move = cpu_figure_with_move
   cpu eng in match fig_with_move with | None -> eng | Some fig ->
   Engine.update_figure eng fig)) | _ -> eng *)
(* let update eng cpu = match (Engine.get_figure eng (get_name cpu))
   with | None -> failwith "DNE" | Some fig -> if Figure.get_name fig =
   "CPU" then init fig else failwith "Not a CPU"*)