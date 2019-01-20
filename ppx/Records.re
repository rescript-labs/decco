open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Parsetree;
open Ast_helper;
open Utils;

type parsedDecl = {
    name: string,
    key: expression, /* "NAME" */
    field: expression, /* v.NAME */
    codecs: (option(expression), option(expression)),
    default: option(expression)
};

let generateEncoder = (decls) => {
    let arrExpr = decls
        |> List.map(({ key, field, codecs: (encoder, _) }) =>
            [%expr ([%e key], [%e BatOption.get(encoder)]([%e field]))]
        )
        |> Exp.array;

    [%expr [%e arrExpr] |> Js.Dict.fromArray |> Js.Json.object_]
        |> Exp.fun_("", None, [%pat? v]);
};

let generateDictGet = ({ key, codecs: (_, decoder), default }) => {
    let decoder = BatOption.get(decoder);
    switch default {
        | Some(default) => [%expr
            switch (Js.Dict.get(dict, [%e key])) {
                | None => Belt.Result.Ok([%e default])
                | Some(json) => [%e decoder](json)
            }
        ];

        | None => [%expr
            switch (Js.Dict.get(dict, [%e key])) {
                | None => Js.Json.null
                | Some(json) => json
            }
            |> [%e decoder]
        ];
    };
};

let generateDictGets = (decls) => decls
    |> List.map(generateDictGet)
    |> tupleOrSingleton(Exp.tuple);

let generateErrorCase = (numDecls, i, { key }) => {
    pc_lhs:
        Array.init(numDecls, which =>
            which === i ? [%pat? Belt.Result.Error(e : Decco.decodeError)] : [%pat? _]
        )
        |> Array.to_list
        |> tupleOrSingleton(Pat.tuple),
    pc_guard: None,
    pc_rhs: [%expr Belt.Result.Error({ ...e, path: "." ++ [%e key] ++ e.path })]
};

let generateSuccessCase = (decls) => {
    pc_lhs: decls
        |> List.map(({ name }) =>
            Location.mknoloc(name)
            |> Pat.var
            |> (p) => [%pat? Belt.Result.Ok([%p p])]
        )
        |> tupleOrSingleton(Pat.tuple),
    pc_guard: None,
    pc_rhs: decls
        |> List.map(({ name }) => (Ast_convenience.lid(name), makeIdentExpr(name)))
        |> (l) => [%expr Belt.Result.Ok([%e Exp.record(l, None)])]
};

let generateDecoder = (decls) => {
    let resultSwitch = List.mapi(generateErrorCase(List.length(decls)), decls)
        |> List.append([generateSuccessCase(decls)])
        |> Exp.match(generateDictGets(decls));

    [%expr (v) =>
        switch (Js.Json.classify(v)) {
            | Js.Json.JSONObject(dict) => [%e resultSwitch]
            | _ => Decco.error("Not an object", v)
        }
    ]
};

let parseDecl = (generatorSettings, { pld_name: { txt }, pld_loc, pld_type, pld_attributes }) => {
    /* If a key is missing from the record on decode, the default (if specified) will be used */
    let default = switch (getAttributeByName(pld_attributes, "decco.default")) {
        | Ok(Some(attribute)) => Some(getExpressionFromPayload(attribute))
        | Ok(None) => None
        | Error(s) => fail(pld_loc, s)
    };

    let key = switch (getAttributeByName(pld_attributes, "decco.key")) {
        | Ok(Some(attribute)) => getExpressionFromPayload(attribute)
        | Ok(None) => Exp.constant(Asttypes.Const_string(txt, None))
        | Error(s) => fail(pld_loc, s)
    };

    {
        name: txt,
        key,
        field: Ast_convenience.lid(txt)
            |> Exp.field([%expr v]),
        codecs: Codecs.generateCodecs(generatorSettings, pld_type),
        default
    };
};

let generateCodecs = ({ doEncode, doDecode } as generatorSettings, decls) => {
    let parsedDecls = List.map(parseDecl(generatorSettings), decls);
    (
        doEncode ? Some(generateEncoder(parsedDecls)) : None,
        doDecode ? Some(generateDecoder(parsedDecls)) : None
    )
};
