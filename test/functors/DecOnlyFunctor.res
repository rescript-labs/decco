module type DecOnly = {
  @decco.decode type t
}
module DecOnly: DecOnly = {
  let t_encode = 1
  @decco.decode type t = int
  ignore(t_encode + 1)
}
