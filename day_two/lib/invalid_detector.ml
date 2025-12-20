(* predicate that checks if the boundary value is an invalid id. *)
let check_boundary s =
  if String.length s mod 2 != 0 then false
  else String.ends_with ~suffix:(String.sub s 0 (String.length s / 2)) s
