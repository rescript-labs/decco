// open Jest
// open TestUtils
// open Expect

// module A = {
//   @decco
//   type t = {first_name: string}
// }

// module B = {
//   @decco
//   type t = {last_name: string}
// }

// module C = {
//   @decco
//   type t = {
//     ...A.t,
//     ...B.t,
//     age: int,
//   }
// }

// describe("record spreading", () => {
//   test("should encode", () => {
//     let a = {
//       first_name: "Bob",
//     }

//     let b = {
//       last_name: "pizza",
//     }

//     let c: c = {
//       first_name: "bob",
//       last_name: "pizza",
//       age: 3,
//     }

//     let encoded = c_encode(c)

//     // expect("123")->toBe(Js.Json.stringify(c_encode(c)))
//     expect("123")->toBe("123")
//   })

//   // test("should decode", () => {
//   //   let encoded = "42"
//   //   let decoded = intAsStr_decode(encoded)
//   //   expect(decoded)->toBe(42)
//   // })
// })

