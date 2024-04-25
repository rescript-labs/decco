open Jest
open TestUtils

@decco type s = string
@decco type l<'a> = list<'a>

describe("list", () => {
  testEncode("l_encode", list{"10", "20"}, l_encode(s_encode), `["10","20"]`)

  describe("l_decode", () => {
    let json = Js.Json.array(Js.Array.map(Js.Json.string, ["10", "20"]))
    testGoodDecode("good", l_decode(s_decode), json, list{"10", "20"})

    describe(
      "bad",
      () => {
        testBadDecode(
          "non-array",
          l_decode(s_decode),
          Js.Json.number(12.),
          {
            message: "Not an array",
            path: "",
            value: Js.Json.number(12.),
          },
        )

        testBadDecode(
          "failed elem",
          l_decode(s_decode),
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
