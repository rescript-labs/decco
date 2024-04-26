module type EncOnly = {
  @decco.encode type t
}
module EncOnly: EncOnly = {
  let t_decode = 1
  @decco.encode type t = int
  ignore(t_decode + 1)
  @@ocaml.doc(" this won't typecheck if t_decode is generated  ")
}
