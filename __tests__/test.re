open Jest;
open Expect;
/* open Decco; /* Don't open these in order to validate ppx works without it */
open Belt.Result; */

[@decco] type s = string;
[@decco] type i = int;
[@decco] type i64 = int64;
[@decco] type i64Unsafe = [@decco.codec Decco.int64Unsafe] int64;
[@decco] type f = float;
[@decco] type b = bool;
[@decco] type u = unit;
[@decco] type t = (int, string);
[@decco] type a('a) = array('a);
[@decco] type l('a) = list('a);
[@decco] type o('a) = option('a);
[@decco] type simpleVar('a) = 'a;
[@decco] type j = Js.Json.t;
[@decco] type optionList = l(o(s));
[@decco] type variant = A | B(i) | C(i, s);
[@decco] type record = {
    hey: s,
    [@decco.default None] ya: o(i),
    opt: option(int)
};

module type TestMod = {
    [@decco] type t;
    [@decco] type varType('a, 'b);

    let mkT : string => t;
    let mkVarType : 'a => 'b => varType('a, 'b);
};

module TestMod : TestMod = {
    [@decco] type t = string;
    [@decco] type varType('a, 'b) = ('a, 'b);

    let mkT = (s : string) : t => s;
    let mkVarType = (a, b) => (a, b);
};

[@decco] type dependentOnTestMod = TestMod.t;

[@decco] type bigV = V(array(option(list(string))));
[@decco] type bigR = {
    bigV: bigV
};

[@decco] type falseable('a) = [@decco.codec Decco.falseable] option('a);

module type EncOnly = {
    [@decco.encode] type t;
};
module EncOnly : EncOnly = {
    type _t = int;
    let _t_encode = Decco.intToJson;
    [@decco.encode] type t = _t;
    let _e = t_decode;
};
EncOnly.t_encode;

module type DecOnly = {
    [@decco.decode] type t;
};
module DecOnly : DecOnly = {
    type _t = int;
    let _t_decode = Decco.intFromJson;
    [@decco.decode] type t = _t;
    let _e = t_decode;
};
DecOnly.t_decode;

let testBadDecode = (name, decode, json, expectedError) =>
    test(name, () => {
        switch (decode(json)) {
            | Belt.Result.Error(e) => expect(e) |> toEqual(expectedError)
            | _ => failwith("Decode erroneously succeeded")
        };
    });

let testGoodDecode = (name, decode, json, expected) =>
    test(name, () =>
        switch (decode(json)) {
            | Belt.Result.Ok(actual) => expect(actual) |> toEqual(expected)
            | Belt.Result.Error({ Decco.path, message }) => failwith({j|Decode error: $message ($path)|j})
        }
    );

describe("string", () => {
    test("s_encode", () => {
        let s = "yeah";
        let json = s_encode(s);
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONString(s2) => expect(s2) |> toBe(s)
            | _ => failwith("Not a JSONString")
        };
    });

    describe("s_decode", () => {
        testGoodDecode("good", s_decode, Js.Json.string("heyy"), "heyy");

        testBadDecode("bad", s_decode, Js.Json.number(12.), {
            path: "",
            message: "Not a string",
            value: Js.Json.number(12.)
        });
    });
});

describe("int", () => {
    test("i_encode", () => {
        let i = 24;
        let json = i_encode(i);
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONNumber(i2) => expect(i2) |> toBe(float_of_int(i))
            | _ => failwith("Not a JSONNumber")
        };
    });

    describe("i_decode", () => {
        testGoodDecode("good", i_decode, Js.Json.number(414.), 414);

        describe("bad", () => {
            let json = Js.Json.string("12.");
            testBadDecode("not a number", i_decode, json, {
                path: "",
                message: "Not a number",
                value: json
            });

            let json = Js.Json.number(5.1);
            testBadDecode("not an int", i_decode, json, {
                path: "",
                message: "Not an integer",
                value: json
            });
        });
    });
});

describe("int64", () => {
    describe("safe", () => {
        let asInt64 = 0x0FFFFFFFFFFFFFFFL;
        let asFloat = 1.2882297539194265e-231;
        test("i64_encode", () => {
            let json = i64_encode(asInt64);
            switch (Js.Json.classify(json)) {
                | Js.Json.JSONNumber(f) => expect(f) |> toBe(asFloat)
                | _ => failwith("Not a JSONNumber")
            };
        });

        describe("i64_decode", () => {
            testGoodDecode("good", i64_decode, Js.Json.number(asFloat), asInt64);

            let json = Js.Json.string("12.");
            testBadDecode("bad", i64_decode, json, {
                path: "",
                message: "Not a number",
                value: json
            });
        });
    });

    describe("unsafe", () => {
        let v = 11806311374010L;
        test("i64Unsafe_encode", () => {
            let json = i64Unsafe_encode(v);
            expect(Js.Json.stringify(json))
                |> toBe({|11806311374010|});
        });

        describe("i64Unsafe_decode", () => {
            let json = Js.Json.number(Int64.to_float(v));
            testGoodDecode("good", i64Unsafe_decode, json, v);

            let json = Js.Json.string("12.");
            testBadDecode("bad", i64Unsafe_decode, json, {
                path: "",
                message: "Not a number",
                value: json
            });
        });
    });
});

describe("float", () => {
    test("f_encode", () => {
        let v = 1.;
        let json = f_encode(v);
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONNumber(v2) => expect(v2) |> toBe(v)
            | _ => failwith("Not a JSONNumber")
        };
    });

    describe("f_decode", () => {
        testGoodDecode("good", f_decode, Js.Json.number(12.), 12.);

        testBadDecode("bad", f_decode, Js.Json.string("12."), {
            path: "",
            message: "Not a number",
            value: Js.Json.string("12.")
        });
    });
});

describe("bool", () => {
    test("b_encode", () =>
        b_encode(true)
            |> Js.Json.classify
            |> expect
            |> toBe(Js.Json.JSONTrue)
    );

    describe("b_decode", () => {
        testGoodDecode("good", b_decode, Js.Json.boolean(false), false);

        testBadDecode("bad", b_decode, Js.Json.string("12."), {
            path: "",
            message: "Not a boolean",
            value: Js.Json.string("12.")
        });
    });
});

describe("unit", () => {
    test("u_encode", () => {
        let json = u_encode();
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONNumber(n) => expect(n) |> toBe(0.)
            | _ => failwith("Not a JSONNumber")
        };
    });

    testGoodDecode("u_decode", u_decode, Js.Json.number(0.), ());
});

describe("tuple", () => {
    test("t_encode", () => {
        let v = (10, "ten");
        let json = t_encode(v);
        expect(Js.Json.stringify(json))
            |> toBe({|[10,"ten"]|});
    });

    describe("t_decode", () => {
        let json = {|[10,"ten"]|} |> Js.Json.parseExn;
        testGoodDecode("good", t_decode, json, (10, "ten"));

        describe("bad", () => {
            let json = Js.Json.number(12.);
            testBadDecode("non-array", t_decode, json, {
                path: "",
                message: "Not a tuple",
                value: json
            });

            let json = {|[10]|} |> Js.Json.parseExn;
            testBadDecode("non-string", t_decode, json, {
                path: "",
                message: "Incorrect cardinality",
                value: json
            });

            let json = {|[10,10]|} |> Js.Json.parseExn;
            testBadDecode("non-string", t_decode, json, {
                path: "[1]",
                message: "Not a string",
                value: Js.Json.number(10.)
            });
        });
    });
});

describe("array", () => {
    test("a_encode", () => {
        let a : a(s) = [|"10", "20"|];
        let json = a_encode(s_encode, a);
        expect(Js.Json.stringify(json))
            |> toBe({|["10","20"]|})
    });

    describe("a_decode", () => {
        let json = [|"10","20"|]
            |> Js.Array.map(Js.Json.string)
            |> Js.Json.array;
        testGoodDecode("good", a_decode(s_decode),json, [|"10","20"|]);

        describe("bad", () => {
            testBadDecode("non-array", a_decode(s_decode), Js.Json.number(12.), {
                message: "Not an array",
                path: "",
                value: Js.Json.number(12.),
            });

            testBadDecode("failed elem", a_decode(s_decode),
                Js.Json.array([|
                    Js.Json.string("str"), Js.Json.number(123.)
                |]), {
                    message: "Not a string",
                    path: "[1]",
                    value: Js.Json.number(123.),
                }
            );
        });
    });
});

describe("list", () => {
    test("l_encode", () => {
        let v = ["10", "20"];
        let json = l_encode(s_encode, v);
        expect(Js.Json.stringify(json))
            |> toBe({|["10","20"]|})
    });

    describe("l_decode", () => {
        let json = [|"10", "20"|]
            |> Js.Array.map(Js.Json.string)
            |> Js.Json.array;
        testGoodDecode("good", l_decode(s_decode), json, ["10", "20"]);

        describe("bad", () => {
            testBadDecode("non-array", l_decode(s_decode), Js.Json.number(12.), {
                message: "Not an array",
                path: "",
                value: Js.Json.number(12.),
            });

            testBadDecode("failed elem", l_decode(s_decode),
                Js.Json.array([|
                    Js.Json.string("str"), Js.Json.number(123.)
                |]), {
                    message: "Not a string",
                    path: "[1]",
                    value: Js.Json.number(123.),
                }
            );
        });
    });
});

describe("option", () => {
    describe("o_encode", () => {
        test("none", () =>
            o_encode(s_encode, None)
                |> Js.Json.classify
                |> expect
                |> toBe(Js.Json.JSONNull)
        );

        test("some", () => {
            let v = Some("yeah");
            let json = o_encode(s_encode, v);
            switch (Js.Json.classify(json)) {
                | Js.Json.JSONString(v2) => expect(v2) |> toBe("yeah")
                | _ => failwith("Not a JSONString")
            };
        });
    });

    describe("o_decode", () => {
        describe("good", () => {
            testGoodDecode("null", o_decode(s_decode), Js.Json.null, None);
            testGoodDecode("non-null", o_decode(s_decode), Js.Json.string("heyy"), Some("heyy"));
        });

        testBadDecode("bad", o_decode(s_decode), Js.Json.number(12.), {
            path: "",
            message: "Not a string",
            value: Js.Json.number(12.)
        });
    });
});

describe("falseable", () => {
    describe("falseable_encode", () => {
        test("none", () =>
            falseable_encode(s_encode, None)
                |> Js.Json.classify
                |> expect
                |> toBe(Js.Json.JSONFalse)
        );

        test("some", () => {
            let v = Some("yeah");
            let json = falseable_encode(s_encode, v);
            switch (Js.Json.classify(json)) {
                | Js.Json.JSONString(v2) => expect(v2) |> toBe("yeah")
                | _ => failwith("Not a JSONString")
            };
        });
    });

    describe("falseable_decode", () => {
        describe("good", () => {
            testGoodDecode("false", falseable_decode(s_decode), Js.Json.boolean(false), None);
            testGoodDecode("non-false", falseable_decode(s_decode), Js.Json.string("heyy"), Some("heyy"));
        });

        testBadDecode("bad", falseable_decode(s_decode), Js.Json.null, {
            path: "",
            message: "Not a string",
            value: Js.Json.null
        });
    });
});

describe("simpleVar", () => {
    test("simpleVar_encode", () => {
        let v : simpleVar(string) = "yeah";
        let json = simpleVar_encode(s_encode, v);
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONString(v2) => expect(v2) |> toBe("yeah")
            | _ => failwith("Not a JSONString")
        };
    });

    describe("simpleVar_decode", () => {
        testGoodDecode("good", simpleVar_decode(s_decode), Js.Json.string("yeah"), "yeah");

        testBadDecode("bad", simpleVar_decode(s_decode), Js.Json.number(12.), {
            path: "",
            message: "Not a string",
            value: Js.Json.number(12.)
        });
    });
});

describe("optionList", () => {
    test("optionList_encode", () => {
        let v = [ Some("a"), None, Some("b") ];
        let json = optionList_encode(v);
        expect(Js.Json.stringify(json))
            |> toBe({|["a",null,"b"]|})
    });

    describe("optionList_decode", () => {
        let json = {|["a",null,"b"]|} |> Js.Json.parseExn;
        testGoodDecode("good", optionList_decode, json, [Some("a"), None, Some("b")]);

        describe("bad", () => {
            testBadDecode("non-array", optionList_decode, Js.Json.number(12.), {
                path: "",
                message: "Not an array",
                value: Js.Json.number(12.)
            });

            let json = {|[null, 3]|} |> Js.Json.parseExn;
            testBadDecode("non-string", optionList_decode, json, {
                path: "[1]",
                message: "Not a string",
                value: Js.Json.number(3.)
            });
        });
    });
});

describe("Js.Json.t", () => {
    test("j_encode", () => {
        let v = Js.Json.string("jay");
        let json = j_encode(v);
        expect(json)
            |> toEqual(v);
    });

    let json = Js.Json.number(12.);
    testGoodDecode("j_decode", j_decode, json, json);
});

describe("variant", () => {
    describe("variant_encode", () => {
        test("A", () => {
            let v = A;
            let json = variant_encode(v);
            expect(Js.Json.stringify(json))
                |> toBe({|["A"]|})
        });
        test("B", () => {
            let v = B(5);
            let json = variant_encode(v);
            expect(Js.Json.stringify(json))
                |> toBe({|["B",5]|})
        });
        test("C", () => {
            let v = C(7, "8");
            let json = variant_encode(v);
            expect(Js.Json.stringify(json))
                |> toBe({|["C",7,"8"]|})
        });
    });

    describe("variant_decode", () => {
        describe("good", () => {
            let json = {|["A"]|} |> Js.Json.parseExn;
            testGoodDecode("A", variant_decode, json, A);
            let json = {|["B",5]|} |> Js.Json.parseExn;
            testGoodDecode("B", variant_decode, json, B(5));
            let json = {|["C",7,"8"]|} |> Js.Json.parseExn;
            testGoodDecode("C", variant_decode, json, C(7, "8"));
        });

        describe("bad", () => {
            testBadDecode("non-variant", variant_decode, Js.Json.number(12.), {
                path: "",
                message: "Not a variant",
                value: Js.Json.number(12.)
            });

            let json = {|["D"]|} |> Js.Json.parseExn;
            testBadDecode("bad constructor", variant_decode, json, {
                path: "",
                message: "Invalid variant constructor",
                value: Js.Json.string("D")
            });

            let json = {|["A",1]|} |> Js.Json.parseExn;
            testBadDecode("too many arguments", variant_decode, json, {
                path: "",
                message: "Invalid number of arguments to variant constructor",
                value: json
            });

            let json = {|["B"]|} |> Js.Json.parseExn;
            testBadDecode("not enough arguments", variant_decode, json, {
                path: "",
                message: "Invalid number of arguments to variant constructor",
                value: json
            });

            let json = {|["B","oh"]|} |> Js.Json.parseExn;
            testBadDecode("invalid argument", variant_decode, json, {
                path: "[0]",
                message: "Not a number",
                value: Js.Json.string("oh")
            });
        });
    });
});

describe("record", () => {
    test("record_encode", () => {
        let v = { hey: "hey", ya: Some(100), opt: Some(99) };
        let json = record_encode(v);
        expect(Js.Json.stringify(json))
            |> toBe({|{"hey":"hey","ya":100,"opt":99}|})
    });

    describe("record_decode", () => {
        describe("good", () => {
            let json = {|{"hey":"hey","ya":100,"opt":99}|} |> Js.Json.parseExn;
            testGoodDecode("base case", record_decode, json, { hey: "hey", ya: Some(100), opt: Some(99) });

            let json = {|{"hey":"hey"}|} |> Js.Json.parseExn;
            testGoodDecode("missing optional", record_decode, json, { hey: "hey", ya: None, opt: None });
        });

        describe("bad", () => {
            testBadDecode("non-object", record_decode, Js.Json.number(12.), {
                path: "",
                message: "Not an object",
                value: Js.Json.number(12.)
            });

            let json = {|{"ya":100}|} |> Js.Json.parseExn;
            testBadDecode("missing field", record_decode, json, {
                path: ".hey",
                message: "Key not found",
                value: json
            });

            let json = {|{"hey":9,"ya":10}|} |> Js.Json.parseExn;
            testBadDecode("invalid field type", record_decode, json, {
                path: ".hey",
                message: "Not a string",
                value: Js.Json.number(9.)
            });
        });
    });
});

describe("Ldot", () => {
    test("dependentOnTestMod_encode", () => {
        let s = TestMod.mkT("yeah");
        let json = dependentOnTestMod_encode(s);
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONString(s2) => expect(TestMod.mkT(s2)) |> toBe(s)
            | _ => failwith("Not a JSONString")
        };
    });

    describe("dependentOnTestMod_decode", () => {
        testGoodDecode("good", dependentOnTestMod_decode, Js.Json.string("heyy"), TestMod.mkT("heyy"));

        testBadDecode("bad", dependentOnTestMod_decode, Js.Json.number(12.), {
            path: "",
            message: "Not a string",
            value: Js.Json.number(12.)
        });
    });
});

describe("TestMod.varType", () => {
    test("varType_encode", () => {
        let s = TestMod.mkVarType(5, "yay");
        let json = TestMod.varType_encode(Decco.intToJson, Decco.stringToJson, s);
        expect(Js.Json.stringify(json))
            |> toBe({|[5,"yay"]|})
    });

    let json = {|[5,"yay"]|} |> Js.Json.parseExn;
    testGoodDecode("varType_decode",
        TestMod.varType_decode(Decco.intFromJson, Decco.stringFromJson),
        json, TestMod.mkVarType(5, "yay")
    );
});

describe("long path", () => {
    test("good", () => {
        let v = { bigV: V([|Some(["yes"])|]) };
        let decoded = bigR_decode(bigR_encode(v));
        switch decoded {
            | Belt.Result.Error(_) => failwith("Decode failure")
            | Belt.Result.Ok(actual) => expect(actual) |> toEqual(v)
        };
    });

    describe("bad", () => {
        let json = {|{"bigV":["V",[null,["","",1]]]}|} |> Js.Json.parseExn;
        testBadDecode("bad", bigR_decode, json, {
            path: ".bigV[0][1][2]",
            message: "Not a string",
            value: Js.Json.number(1.)
        });
    });
});
