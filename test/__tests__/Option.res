open Jest
open Expect
open TestUtils

@decco type s = string
@decco type o<'a> = option<'a>

describe("option", () => {
  describe("o_encode", () => {
    test("none", () => expect(Js.Json.classify(o_encode(s_encode, None)))->toBe(Js.Json.JSONNull))

    test(
      "some",
      () => {
        let v = Some("yeah")
        let json = o_encode(s_encode, v)

        @ocaml.warning("-4")
        switch Js.Json.classify(json) {
        | Js.Json.JSONString(v2) => expect(v2)->toBe("yeah")
        | _ => failwith("Not a JSONString")
        }
      },
    )
  })

  describe("o_decode", () => {
    describe(
      "good",
      () => {
        testGoodDecode("null", o_decode(s_decode, ...), Js.Json.null, None)
        testGoodDecode("undefined", o_decode(s_decode, ...), %raw(`undefined`), None)
        testGoodDecode("non-null", o_decode(s_decode, ...), Js.Json.string("heyy"), Some("heyy"))
      },
    )

    testBadDecode(
      "bad",
      o_decode(s_decode, ...),
      Js.Json.number(12.),
      {
        path: "",
        message: "Not a string",
        value: Js.Json.number(12.),
      },
    )
  })
})
