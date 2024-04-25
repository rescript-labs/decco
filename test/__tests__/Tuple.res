open Jest
open Expect
open TestUtils

@decco type t = (int, string)

describe("tuple", () => {
  testEncode("t_encode", (10, "ten"), t_encode, `[10,"ten"]`)

  describe("t_decode", () => {
    let json = Js.Json.parseExn(`[10,"ten"]`)
    testGoodDecode("good", t_decode, json, (10, "ten"))

    describe(
      "bad",
      () => {
        let json = Js.Json.number(12.)
        testBadDecode(
          "non-array",
          t_decode,
          json,
          {
            path: "",
            message: "Not a tuple",
            value: json,
          },
        )

        let json = Js.Json.parseExn(`[10]`)
        testBadDecode(
          "non-string",
          t_decode,
          json,
          {
            path: "",
            message: "Incorrect cardinality",
            value: json,
          },
        )

        let json = Js.Json.parseExn(`[10,10]`)
        testBadDecode(
          "non-string",
          t_decode,
          json,
          {
            path: "[1]",
            message: "Not a string",
            value: Js.Json.number(10.),
          },
        )
      },
    )
  })
})
