open Jest
open Expect
open TestUtils

@decco type s = string
@decco type simpleVar<'a> = 'a

describe("simpleVar", () => {
  test("simpleVar_encode", () => {
    let v: simpleVar<string> = "yeah"
    let json = simpleVar_encode(s_encode, v)

    @ocaml.warning("-4")
    switch Js.Json.classify(json) {
    | Js.Json.JSONString(v2) => expect(v2)->toBe("yeah")
    | _ => failwith("Not a JSONString")
    }
  })

  describe("simpleVar_decode", () => {
    testGoodDecode("good", simpleVar_decode(s_decode, ...), Js.Json.string("yeah"), "yeah")

    testBadDecode(
      "bad",
      simpleVar_decode(s_decode, ...),
      Js.Json.number(12.),
      {
        path: "",
        message: "Not a string",
        value: Js.Json.number(12.),
      },
    )
  })
})
