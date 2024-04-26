// Let's do a codec that represents numbers as ints at the type level, and strings at runtime
// This can be found in the real world in cases like query strings in URLs

open Jest
open Expect

let intToStr = (i: int) => i->string_of_int
let intFromStr = (s: string) => s->int_of_string

@decco type intAsStr = @decco.codec((intToStr, intFromStr)) int

describe("CustomCodecs", () => {
  test("should encode", () => {
    let x: intAsStr = 42

    let encoded = x->intAsStr_encode
    expect(encoded)->toBe("42")
  })

  test("should decode", () => {
    let encoded = "42"
    let decoded = intAsStr_decode(encoded)
    expect(decoded)->toBe(42)
  })
})
