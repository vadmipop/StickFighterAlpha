type t = {
  points : Vector.t list;
  center : Vector.t;
}

let rec list_string lst =
  match lst with
  | [] -> ""
  | h :: t -> Vector.to_string h ^ " " ^ list_string t

let init points center = { points; center }

let init_rect width height center =
  let w2 = width /. 2. in
  let h2 = height /. 2. in
  {
    points =
      [
        Vector.init w2 h2;
        Vector.init w2 ~-.h2;
        Vector.init ~-.w2 ~-.h2;
        Vector.init ~-.w2 h2;
      ];
    center;
  }

let center poly = poly.center

let side_norms points =
  match points with
  | h :: _ ->
      let rec rec_side_norms points =
        match points with
        | [ v ] -> [ Vector.norm (Vector.sub h v) ]
        | v1 :: v2 :: tail ->
            Vector.norm (Vector.sub v2 v1) :: rec_side_norms (v2 :: tail)
        | _ -> []
      in
      rec_side_norms points
  | _ -> []

let point_in poly p =
  let trans_p = Vector.sub p poly.center in
  let norms = side_norms poly.points in
  let zipped = List.combine poly.points norms in
  let min_dist =
    List.fold_left
      (fun h (point, norm) ->
        Float.max (Vector.dot (Vector.sub trans_p point) norm) h)
      ~-.1. zipped
  in
  min_dist <= 0.

let closest_point p norm poly =
  List.fold_left
    (fun m pt ->
      let trans_pt = Vector.sub (Vector.add pt poly.center) p in
      let dist = Vector.dot norm trans_pt in
      Float.min m dist)
    Float.max_float poly.points

let side_penetration poly1 poly2 =
  let norms = side_norms poly1.points in
  let zipped = List.combine poly1.points norms in
  let dist =
    List.fold_left
      (fun (m, n) (p, norm) ->
        let closest =
          closest_point (Vector.add p poly1.center) norm poly2
        in
        if m >= closest then (m, n) else (closest, norm))
      (~-.Float.max_float, Vector.zero)
      zipped
  in
  dist

let pen_point poly1 poly2 =
  let norms = side_norms poly1.points in
  let rec make_flt_list n = if n > 0 then fst (side_penetration poly1 poly2) :: make_flt_list (n-1) else [] in
  let lst = make_flt_list (List.length norms) in
  let zipped  = List.combine norms lst in
  let dist =
    List.fold_left
      (fun p (norm, m) ->
        let closest =
          closest_point (Vector.add p poly1.center) norm poly2
        in
        if m >= closest then Vector.init (m *. Vector.x p) (m *. Vector.y p) else Vector.init (m *. Vector.x norm) (Vector.x norm))
      Vector.zero
    zipped
  in
  dist

let overlap poly1 poly2 =
  let dist1, norm1 = side_penetration poly1 poly2
  and dist2, norm2 = side_penetration poly2 poly1 in
  dist1 < 0. && dist2 < 0.

let overlap_dir poly poly_in =
  let dist1, norm1 = side_penetration poly poly_in in
  let dist2, norm2 = side_penetration poly_in poly in
  if dist1 < 0. && dist2 < 0. then
    if dist1 > dist2 then Vector.scale ~-.dist1 norm1
    else Vector.scale dist2 norm2
  else Vector.zero

let nearest poly1 poly2 =
  let dist1, norm1 = side_penetration poly1 poly2
  and dist2, norm2 = side_penetration poly2 poly1 in
  if dist1 > dist2 then (dist1, norm1)
  else (dist2, Vector.scale ~-.1. norm2)

let vertices poly = poly.points

let move v poly = { poly with center = Vector.add poly.center v }

let move_to v poly = { poly with center = v }

let reflect poly =
  {
    center = Vector.reflect poly.center;
    points =
      List.fold_left
        (fun acc pt -> Vector.reflect pt :: acc)
        [] poly.points;
  }

let average_center (lst : t list) =
  let rec average_center_aux lst (acc : Vector.t) =
    match lst with
    | [] -> acc
    | h :: t -> average_center_aux t (Vector.add acc h.center)
  in
  average_center_aux lst Vector.zero
