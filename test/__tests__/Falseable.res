open Jest
open Expect
open TestUtils

@decco type s = string
@decco type falseable<'a> = @decco.codec(Decco.Codecs.falseable) option<'a>

describe("falseable", () => {
  describe("falseable_encode", () => {
    test(
      "none",
      () => expect(Js.Json.JSONFalse)->toBe(Js.Json.classify(falseable_encode(s_encode, None))),
    )

    test(
      "some",
      () => {
        let v = Some("yeah")
        let json = falseable_encode(s_encode, v)

        @ocaml.warning("-4")
        switch Js.Json.classify(json) {
        | Js.Json.JSONString(v2) => expect("yeah")->toBe(v2)
        | _ => failwith("Not a JSONString")
        }
      },
    )
  })

  describe("falseable_decode", () => {
    describe(
      "good",
      () => {
        testGoodDecode("false", falseable_decode(s_decode, ...), Js.Json.boolean(false), None)
        testGoodDecode(
          "non-false",
          falseable_decode(s_decode, ...),
          Js.Json.string("heyy"),
          Some("heyy"),
        )
      },
    )

    testBadDecode(
      "bad",
      falseable_decode(s_decode, ...),
      Js.Json.null,
      {
        path: "",
        message: "Not a string",
        value: Js.Json.null,
      },
    )
  })
})
