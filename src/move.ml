open Yojson.Basic.Util

type hitbox =
  | Offense of RigidPoly.t
  | Defense of RigidPoly.t
  | Neutral of RigidPoly.t

type t = {
  name : string;
  frames : int;
  current_frame : int;
  hitboxes : hitbox list;
  force : Vector.t;
  additive : bool * bool;
  damage : float;
  knockback : Vector.t;
  animation : Animation.t;
  where : string;
  reset_where : string;
  reset : bool;
  reset_delay : int;
  current_delay : int;
  changeable : bool;
  direction : Direction.dir;
  controlled : bool;
}

let get_name move = move.name

let get_hurtboxes move =
  let rec offense_boxes = function
    | [] -> []
    | Offense h :: t -> h :: offense_boxes t
    | Defense _ :: t -> offense_boxes t
    | Neutral _ :: t -> offense_boxes t
  in
  offense_boxes move.hitboxes

let get_defense_boxes move =
  let rec defense_boxes = function
    | [] -> []
    | Offense _ :: t -> defense_boxes t
    | Defense h :: t -> h :: defense_boxes t
    | Neutral _ :: t -> defense_boxes t
  in
  defense_boxes move.hitboxes

let get_neutral_boxes move =
  let rec neutral_boxes = function
    | [] -> []
    | Offense _ :: t -> neutral_boxes t
    | Defense _ :: t -> neutral_boxes t
    | Neutral h :: t -> h :: neutral_boxes t
  in
  neutral_boxes move.hitboxes
(* let been_hit move (fig : Figure.t) = (* let hubs = get_hurtboxes move
   in *) let hitbs = Figure.get_hitboxes fig in let rec helper =
   function | [] -> false | h :: t -> if hurting move.hitboxes h then
   true else helper t in helper hitbs *)

let get_kb (move : t) = move.knockback

let get_lst_json str j = j |> member str |> to_list

let rec hubs_from_json hurtboxes j =
  match hurtboxes with
  | [] -> []
  | h :: t ->
      (let poly =
         RigidPoly.init_rect
           (to_float (member "width" h))
           (to_float (member "height" h))
           (Vector.from_json (member "pos" h))
       in
       match to_string (member "type" h) with
       | "offense" -> Offense poly
       | "defense" -> Defense poly
       | _ -> Neutral poly)
      :: hubs_from_json t j

let advance move =
  let current_frame = move.current_frame + 1 in
  if current_frame >= move.frames then None
  else
    Some
      {
        move with
        current_frame = move.current_frame + 1;
        animation =
          (match Animation.advance move.animation with
          | Some ani -> ani
          | None -> move.animation);
      }

let advance_reset move loc =
  let right_loc =
    match (move.reset_where, loc) with
    | "any", _
    | "ground", "ground"
    | "air", "air" ->
        true
    | _, _ -> false
  in
  {
    move with
    current_delay = max (move.current_delay - 1) 0;
    reset = move.reset || right_loc;
  }

let ready move = move.current_delay <= 0 && move.reset

let not_ready move =
  { move with current_delay = move.reset_delay; reset = false }

let reset move =
  {
    move with
    current_frame = 0;
    current_delay = 0;
    animation = Animation.reset move.animation;
  }

let get_animation move = move.animation

let get_where move = move.where

let get_force move = move.force

let additive_force move = move.additive

let get_changeable move = move.changeable

let get_controlled move = move.controlled

let reflect_hb = function
  | Offense p -> Offense (RigidPoly.reflect p)
  | Defense p -> Defense (RigidPoly.reflect p)
  | Neutral p -> Neutral (RigidPoly.reflect p)

let reflect mv =
  {
    mv with
    hitboxes = List.map reflect_hb mv.hitboxes;
    force = Vector.reflect mv.force;
    knockback = Vector.reflect mv.knockback;
    animation = Animation.reflect mv.animation;
    direction = Direction.flip mv.direction;
  }

let of_direction d mv =
  if mv.direction = Direction.Any || d = Direction.Any then mv
  else if mv.direction = d then mv
  else reflect mv

let bool_vec j = (to_bool (member "x" j), to_bool (member "y" j))

let init_from_json j =
  {
    name = to_string (member "name" j);
    frames = to_int (member "frames" j);
    current_frame = 0;
    hitboxes = hubs_from_json (get_lst_json "hitboxes" j) j;
    force = Vector.from_json (member "force" j);
    additive = bool_vec (member "additive_force" j);
    damage = to_float (member "damage" j);
    knockback = Vector.from_json (member "knockback" j);
    animation = Animation.init (member "animation" j);
    where = to_string (member "where" j);
    reset_where = to_string (member "reset" j);
    reset = true;
    reset_delay = to_int (member "reset_delay" j);
    current_delay = 0;
    changeable = to_bool (member "changeable" j);
    direction =
      j |> member "direction" |> to_string |> Direction.of_string;
    controlled = j |> member "controlled" |> to_bool;
  }

let get_effect (move1, pos1) (move2, pos2) =
  let rec loop1 = function
    | [] -> (0., Vector.zero, Vector.zero)
    | h1 :: t1 ->
        let rec loop2 = function
          | [] -> (0., Vector.zero, Vector.zero)
          | h2 :: t2 ->
              let dmg, kb, pen =
                match (h1, h2) with
                | Offense p1, Neutral p2 ->
                    let p1 = RigidPoly.move pos1 p1 in
                    let p2 = RigidPoly.move pos2 p2 in
                    if RigidPoly.overlap p1 p2 then
                      ( move1.damage,
                        move1.knockback,
                        snd (RigidPoly.side_penetration p1 p2) )
                    else (0., Vector.zero, Vector.zero)
                | Offense p1, Defense p2 ->
                    let p1 = RigidPoly.move pos1 p1 in
                    let p2 = RigidPoly.move pos2 p2 in
                    if RigidPoly.overlap p1 p2 then
                      ( move1.damage *. Constants.defense_mult,
                        Vector.scale Constants.defense_mult
                          move1.knockback,
                        snd (RigidPoly.side_penetration p1 p2) )
                    else (0., Vector.zero, Vector.zero)
                | _ -> (0., Vector.zero, Vector.zero)
              in
              let dmg2, kb2, pen2 = loop2 t2 in
              (dmg +. dmg2, Vector.add kb kb2, Vector.add pen pen2)
        in
        let dmg, kb, pen = loop2 move2.hitboxes in
        let dmg2, kb2, pen2 = loop1 t1 in
        (dmg +. dmg2, Vector.add kb kb2, Vector.add pen pen2)
  in
  loop1 move1.hitboxes