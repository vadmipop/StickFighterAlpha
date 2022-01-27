type t = {
  x : float;
  y : float;
}

let init x y = { x; y }

let from_json json =
  {
    x = Yojson.Basic.Util.(to_float (member "x" json));
    y = Yojson.Basic.Util.(to_float (member "y" json));
  }

let x v = v.x

let y v = v.y

let zero = { x = 0.; y = 0. }

let add v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y }

let sub v1 v2 = { x = v1.x -. v2.x; y = v1.y -. v2.y }

let scale c v = { x = c *. v.x; y = c *. v.y }

let dot v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y)

let rotate theta v =
  let s = sin theta in
  let c = cos theta in
  { x = (v.x *. c) -. (v.y *. s); y = (v.x *. s) +. (v.y *. c) }

let length2 v = (v.x *. v.x) +. (v.y *. v.y)

let length v = sqrt (length2 v)

let make_length c v = scale (c /. length v) v

let unitize = make_length 1.

let norm v =
  let l = length v in
  { x = ~-.(v.y /. l); y = v.x /. l }

let constrain_length l r v =
  let len = length v in
  if len < l then make_length l v
  else if len > r then make_length r v
  else v

let constrain_xy (rx, ry) v =
  {
    x = (if v.x > 0. then Float.min rx v.x else Float.max ~-.rx v.x);
    y = (if v.y > 0. then Float.min ry v.y else Float.max ~-.ry v.y);
  }

let to_string v =
  "<" ^ Float.to_string v.x ^ ", " ^ Float.to_string v.y ^ ">"

let to_ints v = (Int.of_float v.x, Int.of_float v.y)

let equal v1 v2 = v1.x = v2.x && v1.y = v2.y

let reflect v = { v with x = ~-.(v.x) }