open Jest
open Expect
open TestUtils

@decco type f = float

describe("float", () => {
  test("f_encode", () => {
    let v = 1.
    let json = f_encode(v)

    @ocaml.warning("-4")
    switch Js.Json.classify(json) {
    | Js.Json.JSONNumber(v2) => expect(v)->toBe(v2)
    | _ => failwith("Not a JSONNumber")
    }
  })

  describe("f_decode", () => {
    testGoodDecode("good", f_decode, Js.Json.number(12.), 12.)

    testBadDecode(
      "bad",
      f_decode,
      Js.Json.string("12."),
      {
        path: "",
        message: "Not a number",
        value: Js.Json.string("12."),
      },
    )
  })
})
