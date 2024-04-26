open Jest
open Expect
open TestUtils

@decco type bigV = V(array<option<list<string>>>)
@decco type bigR = {bigV: bigV}

describe("long path", () => {
  test("good", () => {
    let v = {bigV: V([Some(list{"yes"})])}
    let decoded = bigR_decode(bigR_encode(v))
    switch decoded {
    | Belt.Result.Error(_) => failwith("Decode failure")
    | Belt.Result.Ok(actual) => toEqual(v->expect, actual)
    }
  })

  describe("bad", () => {
    let json = Js.Json.parseExn(`{"bigV":["V",[null,["","",1]]]}`)
    testBadDecode(
      "bad",
      bigR_decode,
      json,
      {
        path: ".bigV[0][1][2]",
        message: "Not a string",
        value: Js.Json.number(1.),
      },
    )
  })
})
