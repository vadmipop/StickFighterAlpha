open Graphics.Screen

type t = {
  position : Vector.t;
  max_speed : float;
  view : float * float;
  bound_x : float * float;
  bound_y : float * float;
}

let init target (width, height) (xmin, xmax) (ymin, ymax) =
  {
    position = target;
    max_speed = Constants.cam_max_speed;
    view = (width, height);
    bound_x = (xmin +. (width /. 2.), xmax -. (width /. 2.));
    bound_y = (ymin +. (height /. 2.), ymax -. (height /. 2.));
  }

(** Determines how fast camera approaches the target. Derivative 1 at x
    = 0, and horizontal assymptote*)
let scale x =
  let a = Constants.cam_scale in
  let c = Constants.cam_const in
  c +. (((a *. (x *. x)) -. (c *. c)) /. (x +. c))

let bound cam =
  let x = Vector.x cam.position in
  let y = Vector.y cam.position in
  let xmin, xmax = cam.bound_x in
  let ymin, ymax = cam.bound_y in
  let x = Float.max (Float.min x xmax) xmin in
  let y = Float.max (Float.min y ymax) ymin in
  { cam with position = Vector.init x y }

let update cam target =
  let diff = Vector.sub target cam.position in
  if Vector.length2 diff < 1. then bound cam
  else
    let vel = Vector.make_length (scale (Vector.length diff)) diff in
    bound { cam with position = Vector.add cam.position vel }

let coords cam screen_dims =
  Vector.sub cam.position (Vector.scale 0.5 screen_dims)
