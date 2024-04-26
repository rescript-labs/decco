open Jest
open Expect
open TestUtils

@decco type u = unit

describe("unit", () => {
  test("u_encode", () => {
    let json = u_encode()

    @ocaml.warning("-4")
    switch Js.Json.classify(json) {
    | Js.Json.JSONNumber(n) => expect(n)->toBe(0.)
    | _ => failwith("Not a JSONNumber")
    }
  })

  testGoodDecode("u_decode", u_decode, Js.Json.number(0.), ())
})
