open Jest
open Expect
open TestUtils
open TestModFunctor

describe("Ldot", () => {
  test("dependentOnTestMod_encode", () => {
    let s = TestMod.mkT("yeah")
    let json = dependentOnTestMod_encode(s)

    @ocaml.warning("-4")
    switch Js.Json.classify(json) {
    | Js.Json.JSONString(s2) => toBe(s->expect, TestMod.mkT(s2))
    | _ => failwith("Not a JSONString")
    }
  })

  describe("dependentOnTestMod_decode", () => {
    testGoodDecode("good", dependentOnTestMod_decode, Js.Json.string("heyy"), TestMod.mkT("heyy"))

    testBadDecode(
      "bad",
      dependentOnTestMod_decode,
      Js.Json.number(12.),
      {
        path: "",
        message: "Not a string",
        value: Js.Json.number(12.),
      },
    )
  })
})
