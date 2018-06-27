open Js.Result;
open ResultEx;

type decodeError = {
    path: string,
    message: string,
    value: Js.Json.t
};

let error = (~path=?, message, value) =>{
    let path = switch path {
        | None => ""
        | Some(s) => s
    };
    Error({ path, message, value });
};

let string_to_json = (s) => Js.Json.string(s);
let string_from_json = (j) =>
    switch (Js.Json.classify(j)) {
        | Js.Json.JSONString(s) => Ok(s)
        | _ => Error({ path: "", message: "Not a string", value: j })
    };

let int_to_json = (i) => i |> float_of_int |> Js.Json.number;
let int_from_json = (j) =>
    switch (Js.Json.classify(j)) {
        | Js.Json.JSONNumber(f) =>
            (float_of_int(Js.Math.floor(f)) == f) ?
                Ok(Js.Math.floor(f))
            : Error({ path: "", message: "Not an integer", value: j })

        | _ => Error({ path: "", message: "Not a number", value: j })
    };

let float_to_json = (v) => v |> Js.Json.number;
let float_from_json = (j) =>
    switch (Js.Json.classify(j)) {
        | Js.Json.JSONNumber(f) => Ok(f)
        | _ => Error({ path: "", message: "Not a number", value: j })
    };

let bool_to_json = (v) => v |> Js.Json.boolean;
let bool_from_json = (j) =>
    switch (Js.Json.classify(j)) {
        | Js.Json.JSONTrue => Ok(true)
        | Js.Json.JSONFalse => Ok(false)
        | _ => Error({ path: "", message: "Not a boolean", value: j })
    };

let unit_to_json = () => Js.Json.number(0.0);
let unit_from_json = (_) => Ok(());

let array_to_json = (encoder, arr) =>
    arr
        |> Js.Array.map(encoder)
        |> Js.Json.array;

let array_from_json = (decoder, json) =>
    switch (Js.Json.classify(json)) {
        | Js.Json.JSONArray(arr) =>
            Js.Array.reducei((acc, jsonI, i) => {
                switch (acc, decoder(jsonI)) {
                    | (Error(_), _) => acc

                    | (_, Error({ path } as error)) =>
                        Error({ ...error, path: "[" ++ string_of_int(i) ++ "]" ++ path })

                    | (Ok(prev), Ok(newVal)) => Ok(Js.Array.concat([|newVal|], prev))
                };
            }, Ok([||]), arr)

        | _ => Error({ path: "", message: "Not an array", value: json })
    };

let list_to_json = (encoder, list) =>
    list
        |> Array.of_list
        |> array_to_json(encoder);

let list_from_json = (decoder, json) =>
    json
        |> array_from_json(decoder)
        |> map(Array.to_list);

let option_to_json = (encoder, opt) =>
    switch opt {
        | Some(x) => encoder(x)
        | None => Js.Json.null
    };

let option_from_json = (decoder, json) =>
    switch (Js.Json.classify(json)) {
        | Js.Json.JSONNull => Ok(None)
        | _ => decoder(json) |> map(v => Some (v))
    };
