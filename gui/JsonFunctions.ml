(** Some common funtions for working with JSONs, to preserve DRY *)

open Yojson.Basic.Util

(** Convert a json 3 array to a color *)
let init_color json =
  match to_list json with
  | [ r; g; b ] -> (to_int r, to_int g, to_int b)
  | _ -> failwith "Not a color"

(** [member_of_default f default json] will run [f json], and either
    return the value, or the default value [default] if an error is
    thrown *)
let member_or_default f d json =
  try f json with
  | Type_error _ -> d
