open Jest
open TestUtils
open TestModFunctor

describe("TestMod.varType", () => {
  testEncode(
    "varType_encode",
    TestMod.mkVarType(5, "yay"),
    TestMod.varType_encode(Decco.intToJson, Decco.stringToJson, ...),
    `[5,"yay"]`,
  )

  let json = Js.Json.parseExn(`[5,"yay"]`)
  testGoodDecode(
    "varType_decode",
    TestMod.varType_decode(Decco.intFromJson, Decco.stringFromJson, ...),
    json,
    TestMod.mkVarType(5, "yay"),
  )
})
