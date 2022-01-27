module Keyset = Set.Make (String)

type t = { mutable pressed : Keyset.t }

let init () = { pressed = Keyset.empty }

let rec update_keys keys =
  match Sdlevent.poll_event () with
  | None -> ()
  | Some e -> (
      match e with
      | Sdlevent.KeyDown k ->
          if k.ke_repeat == 0 then
            keys.pressed <-
              Keyset.add (Sdlkeycode.to_string k.keycode) keys.pressed;
          update_keys keys
      | Sdlevent.KeyUp k ->
          keys.pressed <-
            Keyset.remove (Sdlkeycode.to_string k.keycode) keys.pressed;
          update_keys keys
      | _ -> update_keys keys)

let rec check_keys keys needed =
  (* print_endline "-------------"; List.fold_left (fun () s ->
     print_endline s) () (Keyset.elements keys.pressed); *)
  match needed with
  | [] -> true
  | h :: t ->
      if Keyset.mem h keys.pressed then check_keys keys t else false

let rec remove_keys keys needed =
  match needed with
  | [] -> ()
  | h :: t ->
      keys.pressed <- Keyset.remove h keys.pressed;
      remove_keys keys t

let check_quit keys = Keyset.mem "Escape" keys.pressed
