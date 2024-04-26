open Jest
open TestUtils

@decco type s = string
@decco type i = int
@decco type polyvariant = [#A | #B(i) | #C(i, s)]

describe("polyvariant", () => {
  describe("polyvariant_encode", () => {
    testEncode("A", #A, polyvariant_encode, `["A"]`)
    testEncode("B", #B(5), polyvariant_encode, `["B",5]`)
    testEncode("C", #C(7, "8"), polyvariant_encode, `["C",7,"8"]`)
  })

  describe("polyvariant_decode", () => {
    describe(
      "good",
      () => {
        let json = Js.Json.parseExn(`["A"]`)
        testGoodDecode("A", polyvariant_decode, json, #A)
        let json = Js.Json.parseExn(`["B",5]`)
        testGoodDecode("B", polyvariant_decode, json, #B(5))
        let json = Js.Json.parseExn(`["C",7,"8"]`)
        testGoodDecode("C", polyvariant_decode, json, #C(7, "8"))
      },
    )
    describe(
      "bad",
      () => {
        testBadDecode(
          "non-polyvariant",
          polyvariant_decode,
          Js.Json.number(12.),
          {path: "", message: "Not a polyvariant", value: Js.Json.number(12.)},
        )

        let json = Js.Json.parseExn(`["D"]`)
        testBadDecode(
          "bad constructor",
          polyvariant_decode,
          json,
          {
            path: "",
            message: "Invalid polyvariant constructor",
            value: Js.Json.string("D"),
          },
        )

        let json = Js.Json.parseExn(`["A",1]`)
        testBadDecode(
          "too many arguments",
          polyvariant_decode,
          json,
          {
            path: "",
            message: "Invalid number of arguments to polyvariant constructor",
            value: json,
          },
        )

        let json = Js.Json.parseExn(`[]`)
        testBadDecode(
          "no arguments",
          polyvariant_decode,
          json,
          {
            path: "",
            message: "Expected polyvariant, found empty array",
            value: json,
          },
        )

        let json = Js.Json.parseExn(`["B"]`)
        testBadDecode(
          "not enough arguments",
          polyvariant_decode,
          json,
          {
            path: "",
            message: "Invalid number of arguments to polyvariant constructor",
            value: json,
          },
        )

        let json = Js.Json.parseExn(`["B","oh"]`)
        testBadDecode(
          "invalid argument",
          polyvariant_decode,
          json,
          {path: "[0]", message: "Not a number", value: Js.Json.string("oh")},
        )
      },
    )
  })
})
