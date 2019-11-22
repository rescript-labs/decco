open Jest;
open TestUtils;

[@warning "-33"];
open Belt;
[@warning "+33"];

[@decco] type variant = A | B(int) | C(int, string);

describe("variant", () => {
    describe("variant_encode", () => {
        testEncode("A", A, variant_encode, {|["A"]|});
        testEncode("B", B(5), variant_encode, {|["B",5]|});
        testEncode("C", C(7, "8"), variant_encode, {|["C",7,"8"]|});
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

            let json = {|[]|} |> Js.Json.parseExn;
            testBadDecode("no arguments", variant_decode, json, {
                path: "",
                message: "Expected variant, found empty array",
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
