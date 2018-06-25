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
[@decco] type abc = A | B(int) | C(int, int);

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
