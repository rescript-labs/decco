open Jest
open TestUtils

@decco type i = int
@decco type s = string
@decco type r<'v, 'e> = Belt.Result.t<'v, 'e>

describe("result", () => {
  let enc = r_encode(s_encode, i_encode, ...)
  let dec = r_decode(s_decode, i_decode, ...)

  describe("r_encode", () => {
    testEncode("ok", Belt.Result.Ok("oaky"), enc, `["Ok","oaky"]`)
    testEncode("error", Belt.Result.Error(404), enc, `["Error",404]`)
  })

  describe("r_decode", () => {
    describe(
      "good",
      () => {
        let json = Js.Json.parseExn("[\"Ok\",\"yess\"]")
        testGoodDecode("ok", dec, json, Ok("yess"))

        let json = Js.Json.parseExn("[\"Error\",911]")
        testGoodDecode("error", dec, json, Error(911))
      },
    )

    describe(
      "bad",
      () => {
        let json = Js.Json.number(12.)
        testBadDecode(
          "not an array",
          dec,
          json,
          {
            path: "",
            message: "Not an array",
            value: json,
          },
        )

        let json = Js.Json.parseExn("[]")
        testBadDecode(
          "length != 2",
          dec,
          json,
          {
            path: "",
            message: "Expected exactly 2 values in array",
            value: json,
          },
        )

        let json = Js.Json.parseExn("[0,1]")
        testBadDecode(
          "constructor not a string",
          dec,
          json,
          {
            path: "",
            message: "Not a string",
            value: Js.Json.number(0.),
          },
        )

        let json = Js.Json.parseExn("[\"bad\",1]")
        testBadDecode(
          "unrecognized constructor",
          dec,
          json,
          {
            path: "",
            message: "Expected either \"Ok\" or \"Error\"",
            value: Js.Json.string("bad"),
          },
        )

        let json = Js.Json.parseExn("[\"Ok\",1]")
        testBadDecode(
          "bad Ok decode",
          dec,
          json,
          {
            path: "",
            message: "Not a string",
            value: Js.Json.number(1.),
          },
        )

        let json = Js.Json.parseExn("[\"Error\",null]")
        testBadDecode(
          "bad Error decode",
          dec,
          json,
          {
            path: "",
            message: "Not a number",
            value: Js.Json.null,
          },
        )
      },
    )
  })
})
