open Jest
open Expect
open TestUtils

@decco type s = string

describe("string", () => {
  test("s_encode", () => {
    let s = "yeah"
    let json = s_encode(s)

    @ocaml.warning("-4")
    switch Js.Json.classify(json) {
    | Js.Json.JSONString(s2) => toBe(s->expect, s2)
    | _ => failwith("Not a JSONString")
    }
  })

  describe("s_decode", () => {
    testGoodDecode("good", s_decode, Js.Json.string("heyy"), "heyy")

    testBadDecode(
      "bad",
      s_decode,
      Js.Json.number(12.),
      {
        path: "",
        message: "Not a string",
        value: Js.Json.number(12.),
      },
    )
  })
})
