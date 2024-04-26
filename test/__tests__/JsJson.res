open Jest
open Expect
open TestUtils

@decco type j = Js.Json.t

describe("Js.Json.t", () => {
  test("j_encode", () => {
    let v = Js.Json.string("jay")
    let json = j_encode(v)
    toEqual(v->expect, json)
  })

  let json = Js.Json.number(12.)
  testGoodDecode("j_decode", j_decode, json, json)
})
