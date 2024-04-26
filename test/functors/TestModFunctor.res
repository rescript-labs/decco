module type TestMod = {
  @decco type t
  @decco type varType<'a, 'b>

  let mkT: string => t
  let mkVarType: ('a, 'b) => varType<'a, 'b>
}

module TestMod: TestMod = {
  @decco type t = string
  @decco type varType<'a, 'b> = ('a, 'b)

  let mkT = (s: string): t => s
  let mkVarType = (a, b) => (a, b)
}

@decco type dependentOnTestMod = TestMod.t
