open Jest
open TestUtils

@unboxed
type v = V(string)

@ocaml.warning("-39")
let v_encode = v =>
  (
    v =>
      switch v {
      | V(v0) => Decco.stringToJson(v0)
      }
  )(v)

@ocaml.warning("-4") @ocaml.warning("-39")
and v_decode = v => (v => (Decco.stringFromJson(v)->Belt.Result.map)(v => V(v)))(v)

@unboxed
type r = {r: int}
@ocaml.warning("-39") let r_encode = v => (v => Decco.intToJson(v.r))(v)

@ocaml.warning("-4") @ocaml.warning("-39")
and r_decode = v => (v => (Decco.intFromJson(v)->Belt.Result.map)(v => {r: v}))(v)

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
