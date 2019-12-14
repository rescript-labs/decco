open Jest;
open Expect;

let testBadDecode = (name, decode, json, expectedError) =>
    test(name, () => {
        switch (decode(json)) {
            | Belt.Result.Error(e) => expect(e) |> toEqual(expectedError)
            | Ok(_) => failwith("Decode erroneously succeeded")
        };
    });

let testGoodDecode = (name, decode, json, expected) =>
    test(name, () =>
        switch (decode(json)) {
            | Belt.Result.Ok(actual) => expect(actual) |> toEqual(expected)
            | Belt.Result.Error({ Decco.path, message }) => failwith({j|Decode error: $message ($path)|j})
        }
    );

let testEncode = (name, value, encode, expected) =>
    test(name, () =>
        value
        |> encode
        |> Js.Json.stringify
        |> expect
        |> toBe(expected)
    );
