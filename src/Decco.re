open Belt.Result;

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

let stringToJson = (s) => Js.Json.string(s);
let stringFromJson = (j) =>
    switch (Js.Json.classify(j)) {
        | Js.Json.JSONString(s) => Ok(s)
        | _ => Error({ path: "", message: "Not a string", value: j })
    };

let intToJson = (i) => i |> float_of_int |> Js.Json.number;
let intFromJson = (j) =>
    switch (Js.Json.classify(j)) {
        | Js.Json.JSONNumber(f) =>
            (float_of_int(Js.Math.floor(f)) == f) ?
                Ok(Js.Math.floor(f))
            : Error({ path: "", message: "Not an integer", value: j })

        | _ => Error({ path: "", message: "Not a number", value: j })
    };

let int64ToJson = (i) => i
    |> Int64.float_of_bits
    |> Js.Json.number;

let int64FromJson = (j) =>
    switch (Js.Json.classify(j)) {
        | Js.Json.JSONNumber(n) => Ok(Int64.bits_of_float(n))
        | _ => error("Not a number", j)
    };

let int64ToJsonUnsafe = (i) => i
    |> Int64.to_float
    |> Js.Json.number;

let int64FromJsonUnsafe = (j) =>
    switch (Js.Json.classify(j)) {
        | Js.Json.JSONNumber(n) => Ok(Int64.of_float(n))
        | _ => error("Not a number", j)
    };

let int64Unsafe = (int64ToJsonUnsafe, int64FromJsonUnsafe);

let floatToJson = (v) => v |> Js.Json.number;
let floatFromJson = (j) =>
    switch (Js.Json.classify(j)) {
        | Js.Json.JSONNumber(f) => Ok(f)
        | _ => Error({ path: "", message: "Not a number", value: j })
    };

let boolToJson = (v) => v |> Js.Json.boolean;
let boolFromJson = (j) =>
    switch (Js.Json.classify(j)) {
        | Js.Json.JSONTrue => Ok(true)
        | Js.Json.JSONFalse => Ok(false)
        | _ => Error({ path: "", message: "Not a boolean", value: j })
    };

let unitToJson = () => Js.Json.number(0.0);
let unitFromJson = (_) => Ok(());

let arrayToJson = (encoder, arr) =>
    arr
        |> Js.Array.map(encoder)
        |> Js.Json.array;

let arrayFromJson = (decoder, json) =>
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

let listToJson = (encoder, list) =>
    list
        |> Array.of_list
        |> arrayToJson(encoder);

let listFromJson = (decoder, json) =>
    json
        |> arrayFromJson(decoder)
        |> map(_, Array.to_list);

let optionToJson = (encoder, opt) =>
    switch opt {
        | Some(x) => encoder(x)
        | None => Js.Json.null
    };

let optionFromJson = (decoder, json) =>
    switch (Js.Json.classify(json)) {
        | Js.Json.JSONNull => Ok(None)
        | _ => decoder(json) |> map(_, v => Some(v))
    };

let encodeFalseable = (encoder, opt) =>
    switch opt {
        | None => Js.Json.boolean(false)
        | Some(v) => encoder(v)
    };

let decodeFalseable = (decoder, json) =>
    switch (Js.Json.classify(json)) {
        | Js.Json.JSONFalse => Ok(None)
        | _ => decoder(json) |> map(_, v => Some(v))
    };

let falseable = (encodeFalseable, decodeFalseable);