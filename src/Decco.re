type decodeError = {
    path: string,
    message: string,
    value: Js.Json.t
};

type result('a) = Belt.Result.t('a, decodeError);
type decoder('a) = Js.Json.t => result('a);
type encoder('a) = 'a => Js.Json.t;
type codec('a) = (encoder('a), decoder('a));

let error: 'a. decodeError => result('a) =
  x => Belt.Result.Error(x);

let stringToJson = (s) => Js.Json.string(s);
let stringFromJson = (path, j) =>
    switch (Js.Json.decodeString(j)) {
        | Some(s) => Belt.Result.Ok(s)
        | None => Belt.Result.Error({ path, message: "Not a string", value: j })
    };

let intToJson = (i) => i |> float_of_int |> Js.Json.number;
let intFromJson = (path, j) =>
    switch (Js.Json.decodeNumber(j)) {
        | Some(f) =>
            (float_of_int(Js.Math.floor(f)) == f) ?
                Belt.Result.Ok(Js.Math.floor(f))
            : Belt.Result.Error({ path, message: "Not an integer", value: j })

        | _ => Belt.Result.Error({ path, message: "Not a number", value: j })
    };

let int64ToJson = (i) => i
    |> Int64.float_of_bits
    |> Js.Json.number;

let int64FromJson = (path, j) =>
    switch (Js.Json.decodeNumber(j)) {
        | Some(n) => Belt.Result.Ok(Int64.bits_of_float(n))
        | None => error({ path, message: "Not a number", value: j })
    };

let int64ToJsonUnsafe = (i) => i
    |> Int64.to_float
    |> Js.Json.number;

let int64FromJsonUnsafe = (path, j) =>
    switch (Js.Json.decodeNumber(j)) {
        | Some(n) => Belt.Result.Ok(Int64.of_float(n))
        | None => error({ path, message: "Not a number", value: j })
    };

let floatToJson = (v) => v |> Js.Json.number;
let floatFromJson = (path, j) =>
    switch (Js.Json.decodeNumber(j)) {
        | Some(f) => Belt.Result.Ok(f)
        | None => Belt.Result.Error({ path, message: "Not a number", value: j })
    };

let boolToJson = (v) => v |> Js.Json.boolean;
let boolFromJson = (path, j) =>
    switch (Js.Json.decodeBoolean(j)) {
        | Some(b) => Belt.Result.Ok(b)
        | None => Belt.Result.Error({ path, message: "Not a boolean", value: j })
    };

let unitToJson = () => Js.Json.number(0.0);
let unitFromJson = (_) => Belt.Result.Ok(());

let arrayToJson = (encoder, arr) =>
    arr
    |> Js.Array.map(encoder)
    |> Js.Json.array;

let arrayFromJson = (path, decoder, json) =>
    switch (Js.Json.decodeArray(json)) {
        | Some(arr) =>
            Js.Array.reducei((acc, jsonI, i) => {
                switch (acc, decoder(jsonI)) {
                    | (Belt.Result.Error(_), _) => acc

                    | (_, Belt.Result.Error({ path } as error)) =>
                        Belt.Result.Error({ ...error, path: "[" ++ string_of_int(i) ++ "]" ++ path })

                    | (Belt.Result.Ok(prev), Belt.Result.Ok(newVal)) => Belt.Result.Ok(Js.Array.concat([|newVal|], prev))
                };
            }, Belt.Result.Ok([||]), arr)

        | None => Belt.Result.Error({ path, message: "Not an array", value: json })
    };

let listToJson = (encoder, list) =>
    list
    |> Array.of_list
    |> arrayToJson(encoder);

let listFromJson = (path, decoder, json) =>
    json
    |> arrayFromJson(path, decoder)
    |> Belt.Result.map(_, Array.to_list);

let optionToJson = (encoder, opt) =>
    switch opt {
        | Some(x) => encoder(x)
        | None => Js.Json.null
    };

let optionFromJson = (decoder, json) =>
    switch (Js.Json.decodeNull(json)) {
        | Some(_) => Belt.Result.Ok(None)
        | None => decoder(json) |> Belt.Result.map(_, v => Some(v))
    };

let resultToJson = (okEncoder, errorEncoder, result) =>
    switch result {
        | Belt.Result.Ok(v) => [| Js.Json.string("Ok"), okEncoder(v) |]
        | Belt.Result.Error(e) => [| Js.Json.string("Error"), errorEncoder(e) |]
    }
    |> Js.Json.array;

let resultFromJson = (path, okDecoder, errorDecoder, json) =>
    switch (Js.Json.decodeArray(json)) {
        | Some([| variantConstructorId, payload |]) =>
            switch (Js.Json.decodeString(variantConstructorId)) {
                | Some("Ok") =>
                    okDecoder(payload)
                    -> Belt.Result.map(v => Belt.Result.Ok(v))

                | Some("Error") =>
                    switch(errorDecoder(payload)) {
                        | Belt.Result.Ok(v) => Belt.Result.Ok(Belt.Result.Error(v))
                        | Belt.Result.Error(e) => Belt.Result.Error(e)
                    }

                | Some(_) => error({ path, message: "Expected either \"Ok\" or \"Error\"", value: variantConstructorId})
                | None => error({ path, message: "Not a string", value: variantConstructorId })
            }
        | Some(_) => error({ path, message: "Expected exactly 2 values in array", value: json })
        | None => error({ path, message: "Not an array", value: json })
    };

let dictToJson = (encoder, dict) =>
    dict->Js.Dict.map((. a) => encoder(a), _)->Js.Json.object_;

let dictFromJson = (path, decoder, json) =>
    switch (Js.Json.decodeObject(json)) {
    | Some(dict) =>
        dict
        ->Js.Dict.entries
        ->Belt.Array.reduce(Ok(Js.Dict.empty()), (acc, (key, value)) =>
            switch (acc, decoder(value)) {
                | (Error(_), _) => acc
                | (_, Error({ path } as error)) => Error({...error, path: "." ++ key ++ path})
                | (Ok(prev), Ok(newVal)) =>
                    let () = prev->Js.Dict.set(key, newVal);
                    Ok(prev);
            }
        );
    | None => Error({ path, message: "Not a dict", value: json })
};

module Codecs {
    include Decco_Codecs;
    let string = path => (stringToJson, stringFromJson(path));
    let int = path => (intToJson, intFromJson(path));
    let int64Unsafe = path => (int64ToJsonUnsafe, int64FromJsonUnsafe(path));
    let float = path => (floatToJson, floatFromJson(path));
    let bool = path => (boolToJson, boolFromJson(path));
    let array = path => (arrayToJson, arrayFromJson(path));
    let list = path => (listToJson, listFromJson(path));
    let option = (optionToJson, optionFromJson);
    let unit = (unitToJson, unitFromJson);
};
