open Jest
open TestUtils

@decco type d<'a> = Js.Dict.t<'a>
@decco type dictInt = d<int>

describe("dictInt", () => {
  testEncode(
    "dictInt_encode",
    Js.Dict.fromArray([("foo", 1), ("bar", 2)]),
    dictInt_encode,
    `{"foo":1,"bar":2}`,
  )

  describe("dictInt_decode", () => {
    let json = Js.Json.parseExn(`{"foo":1,"bar":2}`)
    testGoodDecode("good", dictInt_decode, json, Js.Dict.fromArray([("foo", 1), ("bar", 2)]))

    describe(
      "bad",
      () => {
        let badDict = Js.Json.parseExn(`{"foo":1,"bar":"baz"}`)
        testBadDecode(
          "mixed types",
          dictInt_decode,
          badDict,
          {
            path: ".bar",
            message: "Not a number",
            value: Js.Json.string("baz"),
          },
        )
      },
    )
  })
})
