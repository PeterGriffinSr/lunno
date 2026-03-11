let coq_string_to_string cs = String.init (List.length cs) (List.nth cs)
let ocaml_string_to_coq s = List.init (String.length s) (String.get s)
