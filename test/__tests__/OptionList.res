open Jest
open TestUtils

@decco type s = string
@decco type l<'a> = list<'a>
@decco type o<'a> = option<'a>
@decco type optionList = l<o<s>>

describe("optionList", () => {
  testEncode(
    "optionList_encode",
    list{Some("a"), None, Some("b")},
    optionList_encode,
    `["a",null,"b"]`,
  )

  describe("optionList_decode", () => {
    let json = Js.Json.parseExn(`["a",null,"b"]`)
    testGoodDecode("good", optionList_decode, json, list{Some("a"), None, Some("b")})

    describe(
      "bad",
      () => {
        testBadDecode(
          "non-array",
          optionList_decode,
          Js.Json.number(12.),
          {
            path: "",
            message: "Not an array",
            value: Js.Json.number(12.),
          },
        )

        let json = Js.Json.parseExn(`[null, 3]`)
        testBadDecode(
          "non-string",
          optionList_decode,
          json,
          {
            path: "[1]",
            message: "Not a string",
            value: Js.Json.number(3.),
          },
        )
      },
    )
  })
})
