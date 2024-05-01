open Jest
open Expect

module A = {
  @decco
  type t = {first_name: string}
  @decco
  type b = {last_name: string}
}

module C = {
  @decco
  type t = {
    ...A.t,
    ...A.b,
    age: int,
  }
}

describe("record spreading", () => {
  test("should encode", () => {
    let c: C.t = {
      first_name: "bob",
      last_name: "pizza",
      age: 3,
    }

    let encoded = C.t_encode(c)

    expect(Js.Json.stringify(encoded))->toBe(
      {"first_name": "bob", "last_name": "pizza", "age": 3}
      ->Obj.magic
      ->Js.Json.stringify,
    )
  })

  test("should decode", () => {
    let json = Js.Json.parseExn(`{"first_name":"bob","last_name":"pizza","age":3}`)
    let decoded = C.t_decode(json)

    expect(decoded->Belt.Result.map(x => (x.first_name, x.last_name, x.age)))->toEqual(
      Ok(("bob", "pizza", 3)),
    )
  })
})
