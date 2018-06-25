open Decco;
open Js.Result;
open Jest;
open Expect;

[@decco] type s = string;
[@decco] type i = int;
[@decco] type f = float;
[@decco] type b = bool;
[@decco] type u = unit;
[@decco] type a('a) = array('a);
[@decco] type l('a) = list('a);
[@decco] type o('a) = option('a);
[@decco] type simpleVar('a) = 'a;
[@decco] type optionList = list(option(string));
type variant = A | B(i) | C(i, s);

let variant__to_json = (v) =>
    switch v {
        | A => Js.Json.array([|Js.Json.string("A")|])
        | B(v0) => Js.Json.array([|Js.Json.string("B"),i__to_json(v0)|])
        | C(v0, v1) => Js.Json.array([|Js.Json.string("C"),i__to_json(v0),s__to_json(v1)|])
    };

let variant__from_json = (v) =>
    switch (Js.Json.classify(v)) {
        | Js.Json.JSONArray(jsonArr) => {
            let tagged = Js.Array.map(Js.Json.classify, jsonArr);
            switch tagged[0] {
                | Js.Json.JSONString("A") =>
                    (Js.Array.length(tagged) !== 1) ?
                        Decco.error("Invalid number of arguments to variant constructor A", v)
                    :
                        Ok(A)
                | Js.Json.JSONString("B") =>
                    (Js.Array.length(tagged) !== 2) ?
                        Decco.error("Invalid number of arguments to variant constructor B", v)
                    :
                        switch (i__from_json(jsonArr[1])) {
                            | (Error(e)) => Error({ ...e, path: "[0]" ++ e.path })
                            | (Ok(v0)) => Ok(B(v0))
                        };
                | Js.Json.JSONString("C") =>
                    (Js.Array.length(tagged) !== 3) ?
                        Decco.error("Invalid number of arguments to variant constructor C", v)
                    :
                        switch (i__from_json(jsonArr[1]),s__from_json(jsonArr[2])) {
                            | (Error(e), _) => Error({ ...e, path: "[0]" ++ e.path })
                            | (_, Error(e)) => Error({ ...e, path: "[1]" ++ e.path })
                            | (Ok(v0), Ok(v1)) => Ok(C(v0, v1))
                        };
                | _ => Decco.error("Invalid variant constructor", jsonArr[0])
            }
        }

        | _ => Decco.error("Not a tuple", v)
    };

let testBadDecode = (name, decode, json, expectedError) =>
    test(name, () => {
        switch (decode(json)) {
            | Error(e) => expect(e) |> toEqual(expectedError)
            | _ => failwith("Decode erroneously succeeded")
        };
    });

let testGoodDecode = (name, decode, json, expected) =>
    test(name, () =>
        switch (decode(json)) {
            | Ok(actual) => expect(actual) |> toEqual(expected)
            | _ => failwith("Decode error")
        }
    );

describe("string", () => {
    test("s__to_json", () => {
        let s = "yeah";
        let json = s__to_json(s);
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONString(s2) => expect(s2) |> toBe(s)
            | _ => failwith("Not a JSONString")
        };
    });

    describe("s__from_json", () => {
        testGoodDecode("good", s__from_json, Js.Json.string("heyy"), "heyy");

        testBadDecode("bad", s__from_json, Js.Json.number(12.), {
            path: "",
            message: "Not a string",
            value: Js.Json.number(12.)
        });
    });
});

describe("int", () => {
    test("i__to_json", () => {
        let i = 24;
        let json = i__to_json(i);
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONNumber(i2) => expect(i2) |> toBe(float_of_int(i))
            | _ => failwith("Not a JSONNumber")
        };
    });

    describe("i__from_json", () => {
        testGoodDecode("good", i__from_json, Js.Json.number(414.), 414);

        describe("bad", () => {
            let json = Js.Json.string("12.");
            testBadDecode("not a number", i__from_json, json, {
                path: "",
                message: "Not a number",
                value: json
            });

            let json = Js.Json.number(5.1);
            testBadDecode("not an int", i__from_json, json, {
                path: "",
                message: "Not an integer",
                value: json
            });
        });
    });
});

describe("float", () => {
    test("f__to_json", () => {
        let v = 1.;
        let json = f__to_json(v);
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONNumber(v2) => expect(v2) |> toBe(v)
            | _ => failwith("Not a JSONNumber")
        };
    });

    describe("f__from_json", () => {
        testGoodDecode("good", f__from_json, Js.Json.number(12.), 12.);

        testBadDecode("bad", f__from_json, Js.Json.string("12."), {
            path: "",
            message: "Not a number",
            value: Js.Json.string("12.")
        });
    });
});

describe("bool", () => {
    test("b__to_json", () => {
        let json = b__to_json(true);
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONTrue => expect(true) |> toBe(true)
            | _ => failwith("Not JSONTrue")
        };
    });

    describe("b__from_json", () => {
        testGoodDecode("good", b__from_json, Js.Json.boolean(false), false);

        testBadDecode("bad", b__from_json, Js.Json.string("12."), {
            path: "",
            message: "Not a boolean",
            value: Js.Json.string("12.")
        });
    });
});

describe("unit", () => {
    test("u__to_json", () => {
        let json = u__to_json();
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONNumber(n) => expect(n) |> toBe(0.)
            | _ => failwith("Not a JSONNumber")
        };
    });

    testGoodDecode("u__from_json", u__from_json, Js.Json.number(0.), ());
});

describe("array", () => {
    test("a__to_json", () => {
        let a : a(s) = [|"10", "20"|];
        let json = a__to_json(s__to_json, a);
        expect(Js.Json.stringify(json))
            |> toBe({|["10","20"]|})
    });

    describe("a__from_json", () => {
        let json = [|"10","20"|]
            |> Js.Array.map(Js.Json.string)
            |> Js.Json.array;
        testGoodDecode("good", a__from_json(s__from_json),json, [|"10","20"|]);

        describe("bad", () => {
            testBadDecode("non-array", a__from_json(s__from_json), Js.Json.number(12.), {
                message: "Not an array",
                path: "",
                value: Js.Json.number(12.),
            });

            testBadDecode("failed elem", a__from_json(s__from_json),
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
    test("l__to_json", () => {
        let v = ["10", "20"];
        let json = l__to_json(s__to_json, v);
        expect(Js.Json.stringify(json))
            |> toBe({|["10","20"]|})
    });

    describe("l__from_json", () => {
        let json = [|"10", "20"|]
            |> Js.Array.map(Js.Json.string)
            |> Js.Json.array;
        testGoodDecode("good", l__from_json(s__from_json), json, ["10", "20"]);

        describe("bad", () => {
            testBadDecode("non-array", l__from_json(s__from_json), Js.Json.number(12.), {
                message: "Not an array",
                path: "",
                value: Js.Json.number(12.),
            });

            testBadDecode("failed elem", l__from_json(s__from_json),
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
    describe("o__to_json", () => {
        test("none", () => {
            let v = None;
            let json = o__to_json(s__to_json, v);
            switch (Js.Json.classify(json)) {
                | Js.Json.JSONNull => expect(true) |> toBe(true)
                | _ => failwith("Not a JSONNull")
            };
        });

        test("some", () => {
            let v = Some("yeah");
            let json = o__to_json(s__to_json, v);
            switch (Js.Json.classify(json)) {
                | Js.Json.JSONString(v2) => expect(v2) |> toBe("yeah")
                | _ => failwith("Not a JSONString")
            };
        });
    });

    describe("o__from_json", () => {
        describe("good", () => {
            testGoodDecode("null", o__from_json(s__from_json), Js.Json.null, None);
            testGoodDecode("non-null", o__from_json(s__from_json), Js.Json.string("heyy"), Some("heyy"));
        });

        testBadDecode("bad", o__from_json(s__from_json), Js.Json.number(12.), {
            path: "",
            message: "Not a string",
            value: Js.Json.number(12.)
        });
    });
});

describe("simpleVar", () => {
    test("simpleVar__to_json", () => {
        let v : simpleVar(string) = "yeah";
        let json = simpleVar__to_json(s__to_json, v);
        switch (Js.Json.classify(json)) {
            | Js.Json.JSONString(v2) => expect(v2) |> toBe("yeah")
            | _ => failwith("Not a JSONString")
        };
    });

    describe("simpleVar__from_json", () => {
        testGoodDecode("good", simpleVar__from_json(s__from_json), Js.Json.string("yeah"), "yeah");

        testBadDecode("bad", simpleVar__from_json(s__from_json), Js.Json.number(12.), {
            path: "",
            message: "Not a string",
            value: Js.Json.number(12.)
        });
    });
});

describe("optionList", () => {
    test("optionList__to_json", () => {
        let v = [ Some("a"), None, Some("b") ];
        let json = optionList__to_json(v);
        expect(Js.Json.stringify(json))
            |> toBe({|["a",null,"b"]|})
    });

    describe("optionList__from_json", () => {
        let json = {|["a",null,"b"]|} |> Js.Json.parseExn;
        testGoodDecode("good", optionList__from_json, json, [Some("a"), None, Some("b")]);

        describe("bad", () => {
            testBadDecode("non-array", optionList__from_json, Js.Json.number(12.), {
                path: "",
                message: "Not an array",
                value: Js.Json.number(12.)
            });

            let json = {|[null, 3]|} |> Js.Json.parseExn;
            testBadDecode("non-string", optionList__from_json, json, {
                path: "[1]",
                message: "Not a string",
                value: Js.Json.number(3.)
            });
        });
    });
});

describe("variant", () => {
    describe("variant__to_json", () => {
        test("A", () => {
            let v = A;
            let json = variant__to_json(v);
            expect(Js.Json.stringify(json))
                |> toBe({|["A"]|})
        });
        test("B", () => {
            let v = B(5);
            let json = variant__to_json(v);
            expect(Js.Json.stringify(json))
                |> toBe({|["B",5]|})
        });
        test("C", () => {
            let v = C(7, "8");
            let json = variant__to_json(v);
            expect(Js.Json.stringify(json))
                |> toBe({|["C",7,"8"]|})
        });
    });

    describe("variant__from_json", () => {
        describe("good", () => {
            let json = {|["A"]|} |> Js.Json.parseExn;
            testGoodDecode("A", variant__from_json, json, A);
            let json = {|["B",5]|} |> Js.Json.parseExn;
            testGoodDecode("B", variant__from_json, json, B(5));
            let json = {|["C",7,"8"]|} |> Js.Json.parseExn;
            testGoodDecode("C", variant__from_json, json, C(7, "8"));
        });

        describe("bad", () => {
            testBadDecode("non-tuple", variant__from_json, Js.Json.number(12.), {
                path: "",
                message: "Not a tuple",
                value: Js.Json.number(12.)
            });

            let json = {|["D"]|} |> Js.Json.parseExn;
            testBadDecode("bad constructor", variant__from_json, json, {
                path: "",
                message: "Invalid variant constructor",
                value: Js.Json.string("D")
            });

            let json = {|["A",1]|} |> Js.Json.parseExn;
            testBadDecode("bad constructor", variant__from_json, json, {
                path: "",
                message: "Invalid number of arguments to variant constructor A",
                value: json
            });

            let json = {|["B"]|} |> Js.Json.parseExn;
            testBadDecode("bad constructor", variant__from_json, json, {
                path: "",
                message: "Invalid number of arguments to variant constructor B",
                value: json
            });
        });
    });
});
