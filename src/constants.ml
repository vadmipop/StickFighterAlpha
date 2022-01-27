(** All necessary constants for controlling the game *)

(** Debug mode. Only affects graphics, by drawing hitboxes. *)
let debug = false

(** Gravitational accelation *)
let grav = Vector.init 0. ~-.0.7

(** Velocity scale when bouncing off a surface *)
let bounce_dampening = 0.2

(** Velocity scale in the air *)
let air_friction = 0.98

(** Velocity scale on the ground *)
let ground_friction = 0.5

(** right running force *)
let right_force = Vector.init 2.0 0.0

(** left running force *)
let left_force = Vector.scale ~-.1. right_force

(** jump force *)
let jump_force = Vector.init 0.0 12.0

(** how close to the ground to be counted as touching *)
let touching_ground = 4.

(** max speed in the x direction *)
let max_speed_x = 100.

(** max speed in the y direction*)
let max_speed_y = 100.

(** capped speed when applying "controlled" moves *)
let control_speed = 7.

(** max speed of camera *)
let cam_max_speed = 5.

(** constant for determining how camera moves *)
let cam_scale = 0.05

(** constant for determining how camera moves *)
let cam_const = 2.

let defense_mult = 0.0

(** all the lines on a figure's body *)
let figure_lines =
  [
    ("neck", "elbow_left");
    ("neck", "elbow_right");
    ("elbow_left", "hand_left");
    ("elbow_right", "hand_right");
    ("neck", "waist");
    ("waist", "knee_left");
    ("waist", "knee_right");
    ("knee_left", "foot_left");
    ("knee_right", "foot_right");
  ]

(** up vector *)
let up = Vector.init 0. 1.

(** converts damage to a knockback scaler*)
let damage_fn dmg = 0.5 +. (dmg /. 30.)

(** How long to wait before damage can be reapplied *)
let damage_frames = 7

(** The timer controlling the "GAME!" animation *)
let win_timer = 60