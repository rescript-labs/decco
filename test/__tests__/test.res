open Jest
open Expect
open TestUtils
/* open Decco; /* Don't open these in order to validate ppx works without it */
 open Belt.Result; */

@decco type r<'v, 'e> = Belt.Result.t<'v, 'e>
@decco type d<'v> = Js.Dict.t<'v>
@decco type simpleVar<'a> = 'a
@decco type j = Js.Json.t
@decco type optionList = l<o<s>>
@decco type dictInt = d<int>
@decco type variant = A | B(i) | C(i, s)
@decco type polyvariant = [#A | #B(i) | #C(i, s)]
@decco
type record = {
  hey: s,
  opt: option<int>,
  o: o<i>,
  @decco.default(1.0) f: float,
  @decco.key("other_key") otherKey: string,
}

@decco type bigV = V(array<option<list<string>>>)
@decco type bigR = {bigV: bigV}

@decco type falseable<'a> = @decco.codec(Decco.Codecs.falseable) option<'a>
@decco type magic = @decco.codec(Decco.Codecs.magic) int

module type TestMod = {
  @decco type t
  @decco type varType<'a, 'b>

  let mkT: string => t
  let mkVarType: ('a, 'b) => varType<'a, 'b>
}

module TestMod: TestMod = {
  @decco type t = string
  @decco type varType<'a, 'b> = ('a, 'b)

  let mkT = (s: string): t => s
  let mkVarType = (a, b) => (a, b)
}

@decco type dependentOnTestMod = TestMod.t

module type EncOnly = {
  @decco.encode type t
}
module EncOnly: EncOnly = {
  let t_decode = 1
  @decco.encode type t = int
  ignore(t_decode + 1)
  @@ocaml.doc(" this won't typecheck if t_decode is generated  ")
}

module type DecOnly = {
  @decco.decode type t
}
module DecOnly: DecOnly = {
  let t_encode = 1
  @decco.decode type t = int
  ignore(t_encode + 1)
}

describe("result", () => {
  let enc = r_encode(s_encode, i_encode)
  let dec = r_decode(s_decode, i_decode)

  describe("r_encode", () => {
    testEncode("ok", Belt.Result.Ok("oaky"), enc, `["Ok","oaky"]`)
    testEncode("error", Belt.Result.Error(404), enc, `["Error",404]`)
  })

  describe("r_decode", () => {
    describe(
      "good",
      () => {
        let json = Js.Json.parseExn("[\"Ok\",\"yess\"]")
        testGoodDecode("ok", dec, json, Ok("yess"))

        let json = Js.Json.parseExn("[\"Error\",911]")
        testGoodDecode("error", dec, json, Error(911))
      },
    )

    describe(
      "bad",
      () => {
        let json = Js.Json.number(12.)
        testBadDecode(
          "not an array",
          dec,
          json,
          {
            path: "",
            message: "Not an array",
            value: json,
          },
        )

        let json = Js.Json.parseExn("[]")
        testBadDecode(
          "length != 2",
          dec,
          json,
          {
            path: "",
            message: "Expected exactly 2 values in array",
            value: json,
          },
        )

        let json = Js.Json.parseExn("[0,1]")
        testBadDecode(
          "constructor not a string",
          dec,
          json,
          {
            path: "",
            message: "Not a string",
            value: Js.Json.number(0.),
          },
        )

        let json = Js.Json.parseExn("[\"bad\",1]")
        testBadDecode(
          "unrecognized constructor",
          dec,
          json,
          {
            path: "",
            message: "Expected either \"Ok\" or \"Error\"",
            value: Js.Json.string("bad"),
          },
        )

        let json = Js.Json.parseExn("[\"Ok\",1]")
        testBadDecode(
          "bad Ok decode",
          dec,
          json,
          {
            path: "",
            message: "Not a string",
            value: Js.Json.number(1.),
          },
        )

        let json = Js.Json.parseExn("[\"Error\",null]")
        testBadDecode(
          "bad Error decode",
          dec,
          json,
          {
            path: "",
            message: "Not a number",
            value: Js.Json.null,
          },
        )
      },
    )
  })
})

describe("falseable", () => {
  describe("falseable_encode", () => {
    test(
      "none",
      () => toBe(Js.Json.JSONFalse, expect(Js.Json.classify(falseable_encode(s_encode, None)))),
    )

    test(
      "some",
      () => {
        let v = Some("yeah")
        let json = falseable_encode(s_encode, v)

        @ocaml.warning("-4")
        switch Js.Json.classify(json) {
        | Js.Json.JSONString(v2) => toBe("yeah", expect(v2))
        | _ => failwith("Not a JSONString")
        }
      },
    )
  })

  describe("falseable_decode", () => {
    describe(
      "good",
      () => {
        testGoodDecode("false", falseable_decode(s_decode), Js.Json.boolean(false), None)
        testGoodDecode(
          "non-false",
          falseable_decode(s_decode),
          Js.Json.string("heyy"),
          Some("heyy"),
        )
      },
    )

    testBadDecode(
      "bad",
      falseable_decode(s_decode),
      Js.Json.null,
      {
        path: "",
        message: "Not a string",
        value: Js.Json.null,
      },
    )
  })
})

describe("magic", () => {
  let i = 24
  testGoodDecode("", magic_decode, magic_encode(i), i)
})

describe("simpleVar", () => {
  test("simpleVar_encode", () => {
    let v: simpleVar<string> = "yeah"
    let json = simpleVar_encode(s_encode, v)

    @ocaml.warning("-4")
    switch Js.Json.classify(json) {
    | Js.Json.JSONString(v2) => toBe("yeah", expect(v2))
    | _ => failwith("Not a JSONString")
    }
  })

  describe("simpleVar_decode", () => {
    testGoodDecode("good", simpleVar_decode(s_decode), Js.Json.string("yeah"), "yeah")

    testBadDecode(
      "bad",
      simpleVar_decode(s_decode),
      Js.Json.number(12.),
      {
        path: "",
        message: "Not a string",
        value: Js.Json.number(12.),
      },
    )
  })
})

describe("optionList", () => {
  testEncode(
    "optionList_encode",
    list{Some("a"), None, Some("b")},
    optionList_encode,
    `["a",null,"b"]`,
  )

  describe("optionList_decode", () => {
    let json = Js.Json.parseExn(`["a",null,"b"]`)
    testGoodDecode("good", optionList_decode, json, list{Some("a"), None, Some("b")})

    describe(
      "bad",
      () => {
        testBadDecode(
          "non-array",
          optionList_decode,
          Js.Json.number(12.),
          {
            path: "",
            message: "Not an array",
            value: Js.Json.number(12.),
          },
        )

        let json = Js.Json.parseExn(`[null, 3]`)
        testBadDecode(
          "non-string",
          optionList_decode,
          json,
          {
            path: "[1]",
            message: "Not a string",
            value: Js.Json.number(3.),
          },
        )
      },
    )
  })
})

describe("dictInt", () => {
  testEncode(
    "dictInt_encode",
    Js.Dict.fromArray([("foo", 1), ("bar", 2)]),
    dictInt_encode,
    `{"foo":1,"bar":2}`,
  )

  describe("dictInt_decode", () => {
    let json = Js.Json.parseExn(`{"foo":1,"bar":2}`)
    testGoodDecode("good", dictInt_decode, json, Js.Dict.fromArray([("foo", 1), ("bar", 2)]))

    describe(
      "bad",
      () => {
        let badDict = Js.Json.parseExn(`{"foo":1,"bar":"baz"}`)
        testBadDecode(
          "mixed types",
          dictInt_decode,
          badDict,
          {
            path: ".bar",
            message: "Not a number",
            value: Js.Json.string("baz"),
          },
        )
      },
    )
  })
})

describe("Js.Json.t", () => {
  test("j_encode", () => {
    let v = Js.Json.string("jay")
    let json = j_encode(v)
    toEqual(v, expect(json))
  })

  let json = Js.Json.number(12.)
  testGoodDecode("j_decode", j_decode, json, json)
})

describe("variant", () => {
  describe("variant_encode", () => {
    testEncode("A", A, variant_encode, `["A"]`)
    testEncode("B", B(5), variant_encode, `["B",5]`)
    testEncode("C", C(7, "8"), variant_encode, `["C",7,"8"]`)
  })

  describe("variant_decode", () => {
    describe(
      "good",
      () => {
        let json = Js.Json.parseExn(`["A"]`)
        testGoodDecode("A", variant_decode, json, A)
        let json = Js.Json.parseExn(`["B",5]`)
        testGoodDecode("B", variant_decode, json, B(5))
        let json = Js.Json.parseExn(`["C",7,"8"]`)
        testGoodDecode("C", variant_decode, json, C(7, "8"))
      },
    )

    describe(
      "bad",
      () => {
        testBadDecode(
          "non-variant",
          variant_decode,
          Js.Json.number(12.),
          {
            path: "",
            message: "Not a variant",
            value: Js.Json.number(12.),
          },
        )

        let json = Js.Json.parseExn(`["D"]`)
        testBadDecode(
          "bad constructor",
          variant_decode,
          json,
          {
            path: "",
            message: "Invalid variant constructor",
            value: Js.Json.string("D"),
          },
        )

        let json = Js.Json.parseExn(`["A",1]`)
        testBadDecode(
          "too many arguments",
          variant_decode,
          json,
          {
            path: "",
            message: "Invalid number of arguments to variant constructor",
            value: json,
          },
        )

        let json = Js.Json.parseExn(`[]`)
        testBadDecode(
          "no arguments",
          variant_decode,
          json,
          {
            path: "",
            message: "Expected variant, found empty array",
            value: json,
          },
        )

        let json = Js.Json.parseExn(`["B"]`)
        testBadDecode(
          "not enough arguments",
          variant_decode,
          json,
          {
            path: "",
            message: "Invalid number of arguments to variant constructor",
            value: json,
          },
        )

        let json = Js.Json.parseExn(`["B","oh"]`)
        testBadDecode(
          "invalid argument",
          variant_decode,
          json,
          {
            path: "[0]",
            message: "Not a number",
            value: Js.Json.string("oh"),
          },
        )
      },
    )
  })
})

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

describe("record", () => {
  testEncode(
    "record_encode",
    {hey: "hey", opt: Some(100), o: Some(99), f: 1.5, otherKey: "!"},
    record_encode,
    `{"hey":"hey","opt":100,"o":99,"f":1.5,"other_key":"!"}`,
  )

  describe("record_decode", () => {
    describe(
      "good",
      () => {
        let json = Js.Json.parseExn(`{"hey":"hey","opt":100,"o":99,"f":1.5,"other_key":"!"}`)
        testGoodDecode(
          "base case",
          record_decode,
          json,
          {hey: "hey", opt: Some(100), o: Some(99), f: 1.5, otherKey: "!"},
        )

        let json = Js.Json.parseExn(`{"hey":"hey","other_key":"!"}`)
        testGoodDecode(
          "missing optional",
          record_decode,
          json,
          {hey: "hey", opt: None, o: None, f: 1.0, otherKey: "!"},
        )

        let json: Js.Json.t = %raw(`{"hey":"hey","other_key":"!","opt": undefined}`)
        testGoodDecode(
          "optional field set to undefined",
          record_decode,
          json,
          {hey: "hey", opt: None, o: None, f: 1.0, otherKey: "!"},
        )
      },
    )

    describe(
      "bad",
      () => {
        testBadDecode(
          "non-object",
          record_decode,
          Js.Json.number(12.),
          {
            path: "",
            message: "Not an object",
            value: Js.Json.number(12.),
          },
        )

        let json = Js.Json.parseExn(`{"ya":100}`)
        testBadDecode(
          "missing field",
          record_decode,
          json,
          {
            path: ".hey",
            message: "Not a string",
            value: Js.Json.null,
          },
        )

        let json = Js.Json.parseExn(`{"hey":9,"ya":10}`)
        testBadDecode(
          "invalid field type",
          record_decode,
          json,
          {
            path: ".hey",
            message: "Not a string",
            value: Js.Json.number(9.),
          },
        )
      },
    )
  })
})

describe("Ldot", () => {
  test("dependentOnTestMod_encode", () => {
    let s = TestMod.mkT("yeah")
    let json = dependentOnTestMod_encode(s)

    @ocaml.warning("-4")
    switch Js.Json.classify(json) {
    | Js.Json.JSONString(s2) => toBe(s, expect(TestMod.mkT(s2)))
    | _ => failwith("Not a JSONString")
    }
  })

  describe("dependentOnTestMod_decode", () => {
    testGoodDecode("good", dependentOnTestMod_decode, Js.Json.string("heyy"), TestMod.mkT("heyy"))

    testBadDecode(
      "bad",
      dependentOnTestMod_decode,
      Js.Json.number(12.),
      {
        path: "",
        message: "Not a string",
        value: Js.Json.number(12.),
      },
    )
  })
})

describe("TestMod.varType", () => {
  testEncode(
    "varType_encode",
    TestMod.mkVarType(5, "yay"),
    TestMod.varType_encode(Decco.intToJson, Decco.stringToJson),
    `[5,"yay"]`,
  )

  let json = Js.Json.parseExn(`[5,"yay"]`)
  testGoodDecode(
    "varType_decode",
    TestMod.varType_decode(Decco.intFromJson, Decco.stringFromJson),
    json,
    TestMod.mkVarType(5, "yay"),
  )
})

describe("long path", () => {
  test("good", () => {
    let v = {bigV: V([Some(list{"yes"})])}
    let decoded = bigR_decode(bigR_encode(v))
    switch decoded {
    | Belt.Result.Error(_) => failwith("Decode failure")
    | Belt.Result.Ok(actual) => toEqual(v, expect(actual))
    }
  })

  describe("bad", () => {
    let json = Js.Json.parseExn(`{"bigV":["V",[null,["","",1]]]}`)
    testBadDecode(
      "bad",
      bigR_decode,
      json,
      {
        path: ".bigV[0][1][2]",
        message: "Not a string",
        value: Js.Json.number(1.),
      },
    )
  })
})
