open Yojson.Basic.Util
module FigPoints = Map.Make (String)

type fig_points = Vector.t FigPoints.t

type t = {
  current : fig_points list;
  all : fig_points list;
  frames : int;
  looped : bool;
}

type fig = { neck : Vector.t }

let read_points json =
  let all_points = to_assoc json in
  List.fold_left
    (fun mp (s, v) -> FigPoints.add s (Vector.from_json v) mp)
    FigPoints.empty all_points

let rec read_frames = function
  | [] -> []
  | h :: t -> read_points h :: read_frames t

let init json =
  let frames = read_frames (to_list (member "frames" json)) in
  {
    current = frames;
    all = frames;
    frames = List.length frames;
    looped = json |> member "looped" |> to_bool;
  }

let get_point str anim =
  match anim.current with
  | [] -> None
  | h :: _ -> (
      try Some (FigPoints.find str h) with
      | Not_found -> None)

let advance anim =
  let next_frame =
    match anim.current with
    | _ :: t -> t
    | [] -> []
  in
  if next_frame == [] then
    if anim.looped then Some { anim with current = anim.all } else None
  else Some { anim with current = next_frame }

let reset anim = { anim with current = anim.all }

let reflect anim =
  let rec refl_pts = function
    | [] -> []
    | h :: t -> FigPoints.map Vector.reflect h :: refl_pts t
  in
  { anim with current = refl_pts anim.current; all = refl_pts anim.all }
