open Jest
open TestUtils

@unboxed @decco
type v = V(string)
@unboxed @decco
type r = {r: int}

describe("unboxed", () => {
  describe("variant", () => {
    let v = V("xyz")
    let jsonStr = `"xyz"`

    testEncode("encode", v, v_encode, jsonStr)
    testGoodDecode("decode", v_decode, Js.Json.parseExn(jsonStr), v)
  })

  describe("record", () => {
    let v = {r: 101}
    let jsonStr = `101`

    testEncode("encode", v, r_encode, jsonStr)
    testGoodDecode("decode", r_decode, Js.Json.parseExn(jsonStr), v)
  })
})
