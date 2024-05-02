open Jest
open Expect

module A = {
  @decco
  type t = {first_name: string}
  @decco
  type b = {last_name: string}
  module Nested = {
    @decco
    type t = {pizza: string}
  }
}

@decco
type t = {
  ...A.t,
  ...A.b,
  ...A.Nested.t,
  age: int,
}

describe("record spreading", () => {
  test("should encode", () => {
    let v: t = {
      first_name: "bob",
      last_name: "pizza",
      age: 3,
      pizza: "pie",
    }

    let encoded = t_encode(v)

    expect(Js.Json.stringify(encoded))->toBe(
      {"first_name": "bob", "last_name": "pizza", "pizza": "pie", "age": 3}
      ->Obj.magic
      ->Js.Json.stringify,
    )
  })

  test("should decode", () => {
    let json = Js.Json.parseExn(`{"first_name":"bob","last_name":"pizza","age":3, "pizza": "pie"}`)
    let decoded = t_decode(json)

    expect(decoded->Belt.Result.map(x => (x.first_name, x.last_name, x.age, x.pizza)))->toEqual(
      Ok(("bob", "pizza", 3, "pie")),
    )
  })
})
