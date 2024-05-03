let falseableEncode = (encoder, opt) =>
  switch opt {
  | None => Js.Json.boolean(false)
  | Some(v) => encoder(v)
  }
let falseableDecode = (decoder, json) =>
  switch Js.Json.decodeBoolean(json) {
  | Some(false) => Belt.Result.Ok(None)
  | _ => (Belt.Result.mapU(_, v => Some(v)))(decoder(json))
  }
let falseable = (falseableEncode, falseableDecode)

let magicDecode: Decco_types.decoder<'a> = j => Belt.Result.Ok(Obj.magic(j))
let magic: Decco_types.codec<'a> = (x => Obj.magic(x), magicDecode)
