open Jest
open TestUtils

@decco type s = string
@decco type i = int
@decco type o<'a> = option<'a>
@decco
type record = {
  hey: s,
  opt: option<int>,
  o: o<i>,
  @decco.default(1.0) f: float,
  @decco.key("other_key") otherKey: string,
}

describe("record", () => {
  testEncode(
    "record_encode",
    {hey: "hey", opt: Some(100), o: Some(99), f: 1.5, otherKey: "!"},
    record_encode,
    `{"hey":"hey","opt":100,"o":99,"f":1.5,"other_key":"!"}`,
  )

  describe("record_decode", () => {
    describe(
      "good",
      () => {
        let json = Js.Json.parseExn(`{"hey":"hey","opt":100,"o":99,"f":1.5,"other_key":"!"}`)
        testGoodDecode(
          "base case",
          record_decode,
          json,
          {hey: "hey", opt: Some(100), o: Some(99), f: 1.5, otherKey: "!"},
        )

        let json = Js.Json.parseExn(`{"hey":"hey","other_key":"!"}`)
        testGoodDecode(
          "missing optional",
          record_decode,
          json,
          {hey: "hey", opt: None, o: None, f: 1.0, otherKey: "!"},
        )

        let json: Js.Json.t = %raw(`{"hey":"hey","other_key":"!","opt": undefined}`)
        testGoodDecode(
          "optional field set to undefined",
          record_decode,
          json,
          {hey: "hey", opt: None, o: None, f: 1.0, otherKey: "!"},
        )
      },
    )

    describe(
      "bad",
      () => {
        testBadDecode(
          "non-object",
          record_decode,
          Js.Json.number(12.),
          {
            path: "",
            message: "Not an object",
            value: Js.Json.number(12.),
          },
        )

        let json = Js.Json.parseExn(`{"ya":100}`)
        testBadDecode(
          "missing field",
          record_decode,
          json,
          {
            path: ".hey",
            message: "Not a string",
            value: Js.Json.null,
          },
        )

        let json = Js.Json.parseExn(`{"hey":9,"ya":10}`)
        testBadDecode(
          "invalid field type",
          record_decode,
          json,
          {
            path: ".hey",
            message: "Not a string",
            value: Js.Json.number(9.),
          },
        )
      },
    )
  })
})
