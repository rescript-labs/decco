open Belt.Result;

let falseableEncode = (encoder, opt) =>
    switch opt {
        | None => Js.Json.boolean(false)
        | Some(v) => encoder(v)
    };

let falseableDecode = (decoder, json) =>
    switch (Js.Json.classify(json)) {
        | Js.Json.JSONFalse => Ok(None)
        | _ => decoder(json)
            |> map(_, v => Some(v))
    };


let falseable = (falseableEncode, falseableDecode);

let magicDecode = (j) => Ok(Obj.magic(j));

let magic = (Obj.magic, magicDecode);
