open Jest
open Expect

let testBadDecode = (name, decode, json, expectedError) =>
  test(name, () =>
    switch decode(json) {
    | Belt.Result.Error(e) => expect(expectedError)->toEqual(e)
    | Ok(_) => failwith("Decode erroneously succeeded")
    }
  )

let testGoodDecode = (name, decode, json, expected) =>
  test(name, () =>
    switch decode(json) {
    | Belt.Result.Ok(actual) => expect(expected)->toEqual(actual)
    | Belt.Result.Error({Decco.path: path, message}) =>
      failwith(`Decode error: ${message} (${path})`)
    }
  )

let testEncode = (name, value, encode, expected: string) =>
  test(name, () => expect(expected)->toBe(Js.Json.stringify(encode(value))))
