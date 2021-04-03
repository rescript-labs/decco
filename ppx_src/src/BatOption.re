/* TODO: figure out why I can't compile ppx on newer Ocaml version */

let get = fun
    | None => failwith("Expected Some. got None")
    | Some(v) => v;

let some = (v) => Some(v);
