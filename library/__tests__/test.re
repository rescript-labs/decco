open Decco;
open Js.Result;
open Jest;
open Expect;

[@decco] type s = string;
[@decco] type i = int;
[@decco] type f = float;
[@decco] type b = bool;
[@decco] type u = unit;
[@deccjo] type a('a) = array('a);
type l('a) = list('a);
type o('a) = option('a);

type simpleVar('a) = 'a;

let s__from_json = (j) => string_from_json(j);

let i__to_json = (v) => int_to_json(v);
let i__from_json = (j) => int_from_json(j);

let f__to_json = (v) => float_to_json(v);
let f__from_json = (j) => float_from_json(j);

let b__to_json = (v) => bool_to_json(v);
let b__from_json = (j) => bool_from_json(j);

let u__to_json = (v) => unit_to_json(v);
let u__from_json = (j) => unit_from_json(j);

let a__to_json = (encoder, v) => array_to_json(encoder, v);
let a__from_json = (decoder, j) => array_from_json(decoder, j);

let l__to_json = (encoder, v) => list_to_json(encoder, v);
let l__from_json = (decoder, j) => list_from_json(decoder, j);

let o__to_json = (encoder, v) => option_to_json(encoder, v);
let o__from_json = (decoder, j) => option_from_json(decoder, j);

let testBadDecode = (name, decode, json, expectedError) =>
    test(name, () => {
        switch (decode(json)) {
            | Error(e) => expect(e) |> toEqual(expectedError)
            | _ => failwith("Decode erroneously succeeded")
        };
    });

let testGoodDecode = (name, json, decode, expected) =>
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
        testGoodDecode("good", Js.Json.string("heyy"), s__from_json, "heyy");

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
        testGoodDecode("good", Js.Json.number(414.), i__from_json, 414);

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
        testGoodDecode("good", Js.Json.number(12.), f__from_json, 12.);

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
        testGoodDecode("good", Js.Json.boolean(false), b__from_json, false);

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

    testGoodDecode("u__from_json", Js.Json.number(0.), u__from_json, ());
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
        testGoodDecode("good", json, a__from_json(s__from_json), [|"10","20"|]);

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
        testGoodDecode("good", json, l__from_json(s__from_json), ["10", "20"]);

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
            testGoodDecode("null", Js.Json.null, o__from_json(s__from_json), None);
            testGoodDecode("non-null", Js.Json.string("heyy"), o__from_json(s__from_json), Some("heyy"));
        });

        testBadDecode("bad", o__from_json(s__from_json), Js.Json.number(12.), {
            path: "",
            message: "Not a string",
            value: Js.Json.number(12.)
        });
    });
});
