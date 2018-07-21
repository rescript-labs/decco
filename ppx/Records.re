open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Parsetree;
open Ast_helper;
open Longident;
open Utils;

type parsedDecl = {
    name: string,
    str: expression, /* "NAME" */
    field: expression, /* v.NAME */
    codecs: (expression, expression),
    default: option(expression)
};

let generateEncoder = (decls) => {
    let arrExpr = decls
        |> List.map(({ str, field, codecs: (encoder, _) }) =>
            [%expr ([%e str], [%e encoder]([%e field]))]
        )
        |> Exp.array;

    [%expr [%e arrExpr] |> Js.Dict.fromArray |> Js.Json.object_]
        |> Exp.fun_("", None, [%pat? v]);
};

let generateDictGet = ({ str, codecs: (_, decoder), default }) => {
    switch default {
        | Some(default) => [%expr
            switch (Js.Dict.get(dict, [%e str])) {
                | None => Belt.Result.Ok([%e default])
                | Some(json) => [%e decoder](json)
            }
        ];

        | None => [%expr
            switch (Js.Dict.get(dict, [%e str])) {
                | None => Decco.error("Key not found", v)
                | Some(json) => [%e decoder](json)
            }
        ];
    };
};

let generateDictGets = (decls) => decls
    |> List.map(generateDictGet)
    |> tupleOrSingleton(Exp.tuple);

let generateErrorCase = (numDecls, i, { str }) => {
    pc_lhs:
        Array.init(numDecls, which =>
            which === i ? [%pat? Belt.Result.Error(e : Decco.decodeError)] : [%pat? _]
        )
        |> Array.to_list
        |> tupleOrSingleton(Pat.tuple),
    pc_guard: None,
    pc_rhs: [%expr Belt.Result.Error({ ...e, path: "." ++ [%e str] ++ e.path })]
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

let parseDecl = ({ pld_name: { txt }, pld_loc, pld_type, pld_attributes }) => {
    /* If a key is missing from the record on decode, the default (if specified) will be used */
    let defaultDecls = getAttributeByName(pld_attributes, "decco.default");
    let default = switch (defaultDecls, pld_type.ptyp_desc) {
        | (Ok(Some(attribute)), _) => Some(getExpressionFromPayload(attribute))

        /* Set default for option to None */
        | (Ok(None), Ptyp_constr({ txt: Lident("option") }, _)) => Some([%expr None])

        | (Ok(None), _) => None
        | (Error(s), _) => fail(pld_loc, s)
    };

    {
        name: txt,
        str: Exp.constant(Asttypes.Const_string(txt, None)),
        field: Ast_convenience.lid(txt)
            |> Exp.field([%expr v]),
        codecs: Codecs.generateCodecs(pld_type),
        default
    };
};

let generateCodecs = (decls) => {
    let parsedDecls = List.map(parseDecl, decls);
    (generateEncoder(parsedDecls), generateDecoder(parsedDecls))
};
