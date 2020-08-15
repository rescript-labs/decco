open Migrate_parsetree;
open Ast_406;
open Ppx_tools_406;
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

let generateEncoder = (decls, unboxed) => {
    unboxed
        ? {
            let { codecs, field } = List.hd(decls);
            let (e, _) = codecs;
            [%expr (v) => [%e BatOption.get(e)]([%e field])]
        } : {
            let arrExpr =
                decls
                |> List.map(({ key, field, codecs: (encoder, _) }) =>
                    [%expr ([%e key], [%e BatOption.get(encoder)]([%e field]))]
                )
                |> Exp.array;
            [%expr [%e arrExpr] |> Js.Dict.fromArray |> Js.Json.object_]
            |> Exp.fun_(Asttypes.Nolabel, None, [%pat? v])
        }
};

let generateDictGet = ({ key, codecs: (_, decoder), default }) => {
    let decoder = BatOption.get(decoder);
    switch default {
        | Some(default) => [%expr
            Js.Dict.get(dict, [%e key])
            -> Belt.Option.map([%e decoder])
            -> Belt.Option.getWithDefault(Belt.Result.Ok([%e default]))
        ];

        | None => [%expr
            Js.Dict.get(dict, [%e key])
            -> Belt.Option.getWithDefault(Js.Json.null)
            |> [%e decoder]
        ];
    };
};

let generateDictGets = (decls) => decls
    |> List.map(generateDictGet)
    |> tupleOrSingleton(Exp.tuple);

let generateErrorCase = ({ key }) => {
    pc_lhs: [%pat? Belt.Result.Error(e : Decco.decodeError)],
    pc_guard: None,
    pc_rhs: [%expr Belt.Result.Error({ ...e, path: "." ++ [%e key] ++ e.path })]
};

let generateFinalRecordExpr = (decls) =>
    decls
    |> List.map(({ name }) => (Ast_convenience.lid(name), makeIdentExpr(name)))
    |> (l) => [%expr Belt.Result.Ok([%e Exp.record(l, None)])];

let generateSuccessCase = ({ name }, successExpr) => {
    pc_lhs:
        Location.mknoloc(name)
        |> Pat.var
        |> (p) => [%pat? Belt.Result.Ok([%p p])],
    pc_guard: None,
    pc_rhs: successExpr
};

/** Recursively generates an expression containing nested switches, first
 *  decoding the first record items, then (if successful) the second, etc. */
let rec generateNestedSwitchesRecurse = (allDecls, remainingDecls) => {
    let (current, successExpr) = switch remainingDecls {
        | [] => failwith("Decco internal error: [] not expected")
        | [last, ...[]] => (last, generateFinalRecordExpr(allDecls))
        | [first, ...tail] => (first, generateNestedSwitchesRecurse(allDecls, tail))
    };

    [generateErrorCase(current)]
    |> List.append([generateSuccessCase(current, successExpr)])
    |> Exp.match(generateDictGet(current));
};

let generateNestedSwitches = (decls) => generateNestedSwitchesRecurse(decls, decls);

let generateDecoder = (decls, unboxed) => {
    unboxed
        ? {
            let { codecs, name } = List.hd(decls);
            let (_, d) = codecs;

            let recordExpr =
                [(Ast_convenience.lid(name), makeIdentExpr("v"))]
                |> Exp.record(_, None);

            [%expr (v) =>
                [%e BatOption.get(d)](v)
                -> Belt.Result.map(v => [%e recordExpr])
            ]
        } :
            [%expr (v) =>
                switch (Js.Json.classify(v)) {
                    | Js.Json.JSONObject(dict) => [%e generateNestedSwitches(decls)]
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
        | Ok(None) => Exp.constant(Pconst_string(txt, None))
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

let generateCodecs = ({ doEncode, doDecode } as generatorSettings, decls, unboxed) => {
    let parsedDecls = List.map(parseDecl(generatorSettings), decls);
    (
        doEncode ? Some(generateEncoder(parsedDecls, unboxed)) : None,
        doDecode ? Some(generateDecoder(parsedDecls, unboxed)) : None
    )
};
