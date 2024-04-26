open Jest
open TestUtils

@decco type s = string
@decco type a<'a> = array<'a>

describe("array", () => {
  testEncode("a_encode", ["10", "20"], a_encode(s_encode, ...), `["10","20"]`)

  describe("a_decode", () => {
    let json = Js.Json.array(Js.Array.map(Js.Json.string, ["10", "20"]))
    testGoodDecode("good", a_decode(s_decode, ...), json, ["10", "20"])

    describe(
      "bad",
      () => {
        testBadDecode(
          "non-array",
          a_decode(s_decode, ...),
          Js.Json.number(12.),
          {
            message: "Not an array",
            path: "",
            value: Js.Json.number(12.),
          },
        )

        testBadDecode(
          "failed elem",
          a_decode(s_decode, ...),
          Js.Json.array([Js.Json.string("str"), Js.Json.number(123.)]),
          {
            message: "Not a string",
            path: "[1]",
            value: Js.Json.number(123.),
          },
        )
      },
    )
  })
})
