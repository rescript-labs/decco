open Jest
open Expect
open TestUtils

@decco type b = bool

describe("bool", () => {
  test("b_encode", () => expect(Js.Json.JSONTrue)->toBe(Js.Json.classify(b_encode(true))))

  describe("b_decode", () => {
    testGoodDecode("good", b_decode, Js.Json.boolean(false), false)

    testBadDecode(
      "bad",
      b_decode,
      Js.Json.string("12."),
      {
        path: "",
        message: "Not a boolean",
        value: Js.Json.string("12."),
      },
    )
  })
})
