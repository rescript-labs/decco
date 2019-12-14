let falseableEncode = (encoder, opt) =>
    switch opt {
        | None => Js.Json.boolean(false)
        | Some(v) => encoder(v)
    };
let falseableDecode = (decoder, json) =>
    switch (Js.Json.decodeBoolean(json)) {
        | Some(false) => Belt.Result.Ok(None)
        | _ =>
            decoder(json)
            |> Belt.Result.map(_, v => Some(v))
    };
let falseable = (falseableEncode, falseableDecode);

let magicDecode = (j) => Belt.Result.Ok(Obj.magic(j));
let magic = (Obj.magic, magicDecode);
