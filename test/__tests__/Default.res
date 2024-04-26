open Jest
open Expect

@decco
type record = {@decco.default("default") s: string}

describe("Decco @decco.default", () => {
  test("should use default value when key is missing", () => {
    let json = Js.Json.parseExn("{}")
    let decoded = record_decode(json)

    switch decoded {
    | Belt.Result.Ok(record) => expect(record.s)->toEqual("default")
    | Belt.Result.Error(_) => fail("Decoding failed")
    }
  })
})
