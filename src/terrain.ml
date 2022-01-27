type t = { polys : RigidPoly.t list }

let init polys = { polys }

let add_body ter poly = { polys = poly :: ter.polys }

let get_polys ter = ter.polys