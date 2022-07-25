# Decco

## Project Status

Decco is not being actively maintained, but PRs will be accepted and appreciated.

## What is it?

A Bucklescript PPX which generates JSON serializers and deserializers for user-defined types.

Example:

```reason
/* Define types */
[@decco] type variant('a) = A | B(int) | C(int, 'a);

/*
 * Rescript@9+ users do not use [brackets] for the ppx: 
 * @decco type variant('a) = A | B(int) | C(int, 'a) 
 */

type dict = Js.Dict.t(string);
[@decco] type mytype = {
    s: string,
    i: int,
    o: option(int),
    complex: array(option(list(variant(string)))),
    [@decco.default 1.0] f: float,
    [@decco.key "other_key"] otherKey: string,
    magic: [@decco.codec Decco.Codecs.magic] dict,
};

/* Use <typename>_encode to encode */
let encoded = mytype_encode({
    s: "hello",
    i: 12,
    o: None,
    complex: [| Some([ C(25, "bullseye") ]) |],
    f: 13.,
    otherKey: "other",
    magic: Js.Dict.fromArray([|("key","value")|]),
});

Js.log(Js.Json.stringifyWithSpace(encoded, 2));
/* {
     "s": "hello",
     "i": 12,
     "o": null,
     "complex": [ [ ["C", 25, "bullseye"] ] ],
     "f": 13,
     "other_key": "other",
     "magic": { "key": "value" }
  } */

/* Use <typename>_decode to decode */
let { s, i, o, complex, f, otherKey, magic } =
    mytype_decode(encoded)
    |> Belt.Result.getExn;
```

## How do I install it?
1. Install package
```
npm i decco
```

2. Update your `bsconfig.json`
```json
{
  ...,
  "bs-dependencies": [ "decco" ],
  "ppx-flags": [ "decco/ppx" ],
  ...
}
```

Adding `decco/ppx` to `ppx-flags` will enable the PPX. Adding decco to `bs-dependencies` is required because the code generated by the PPX references the `Decco` module.

**Note:** If you need to use decco with BuckleScript 5, install `@ryb73/decco` version ^0.1.0 by [following the old ReadMe here](https://github.com/reasonml-labs/decco/blob/0452fc42fa4cd4230d394c718e7f62a0384ce045/README.md).

## How do I use it?

See [`test.re`](test/__tests__/test.re) for some examples.

## Reference
### Attributes
#### [@decco]
Applies to: type declarations, type signatures

Indicates that an encoder and decoder should be generated for the given type.

#### [@decco.encode]
Applies to: type declarations, type signatures

Indicates than an encoder (but no decoder) should be generated for the given type.

#### [@decco.decode]
Applies to: type declarations, type signatures

Indicates than an decoder (but no encoder) should be generated for the given type.

#### [@decco.codec]
Applies to: type expressions

Specifies custom encoders and decoders for the type. Note that both an encoder and decoder must be specified, even if the type expression is within a type for which [@decco.encode] or [@decco.decode] was specified.

```reason
[@decco] type t = [@decco.codec (fancyEncoder, fancyDecoder)] fancyType;
```

#### [@decco.key]
Applies to: record fields

By default, Reason record fields map to JS object fields of the same name. Use [@decco.key] to specify a custom JS field name. Useful if the JS field name is invalid as a Reason record field name.

```reason
[@decco]
type record = {
    [@decco.key "other_key"] otherKey: string,
};
```

#### [@decco.default]
Applies to: record fields
Default: `Js.Json.null`

When decoding a record, the default value will be used for keys that are missing from the JSON object being decoded.

```reason
[@decco] type record = {
    [@decco.default "def"] s: string,
};

let {s} = Js.Json.parseExn("{}") |> record_decode |> Belt.Result.getExn;
Js.log(s); /* def */
```

# Contributing

Please read the [CONTRIBUTING.md](./CONTRIBUTING.md)
