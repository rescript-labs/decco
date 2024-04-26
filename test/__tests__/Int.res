open Jest
open Expect
open TestUtils

@decco type i = int

describe("int", () => {
  test("i_encode", () => {
    let i = 24
    let json = i_encode(i)

    @ocaml.warning("-4")
    switch Js.Json.classify(json) {
    | Js.Json.JSONNumber(i2) => expect(float_of_int(i))->toBe(i2)
    | _ => failwith("Not a JSONNumber")
    }
  })

  describe("i_decode", () => {
    testGoodDecode("good", i_decode, Js.Json.number(414.), 414)

    describe(
      "bad",
      () => {
        let json = Js.Json.string("12.")
        testBadDecode(
          "not a number",
          i_decode,
          json,
          {
            path: "",
            message: "Not a number",
            value: json,
          },
        )

        let json = Js.Json.number(5.1)
        testBadDecode(
          "not an int",
          i_decode,
          json,
          {
            path: "",
            message: "Not an integer",
            value: json,
          },
        )
      },
    )
  })
})
