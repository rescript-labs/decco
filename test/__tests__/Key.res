open Jest
open Expect

@decco
type record = {@decco.key("customKey") s: string}

describe("Decco @decco.key", () => {
  test("should decode value from custom key", () => {
    let json = Js.Json.parseExn(`{"customKey": "value"}`)
    let decoded = record_decode(json)

    switch decoded {
    | Belt.Result.Ok(record) => expect(record.s)->toEqual("value")
    | Belt.Result.Error(_) => fail("Decoding failed")
    }
  })
})
