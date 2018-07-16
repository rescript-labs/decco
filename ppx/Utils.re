open Migrate_parsetree;
open Ast_402;
open Parsetree;
open Ast_helper;

type result('a, 'b) = Ok('a) | Error('b);

let encoderFuncSuffix = "__to_json";
let decoderFuncSuffix = "__from_json";
let encoderVarPrefix = "encoder_";
let decoderVarPrefix = "decoder_";

let fail = (loc, message) =>
    Location.error(~loc, message)
        |> (v) => Location.Error(v)
        |> raise;

let makeIdentExpr = (s) =>
    Longident.parse(s)
        |> Location.mknoloc
        |> Exp.ident;

let tupleOrSingleton = (tuple, l) =>
    List.length(l) > 1 ? tuple(l) : List.hd(l);

let getAttributeByName = (attributes, name) => {
    let filtered = attributes
        |> List.filter((({ Location.txt }, _)) => txt == name);

    switch filtered {
        | [] => Ok(None)
        | [attribute] => Ok(Some(attribute))
        | _ => Error("Too many occurrences of \"" ++ name ++ "\" attribute")
    };
};

let getExpressionFromPayload = (({ loc } : Location.loc('a), payload)) =>
    switch payload {
        | PStr([{ pstr_desc }]) => switch pstr_desc {
            | Pstr_eval(expr, _) => expr
            | _ => fail(loc, "Expected expression as attribute payload")
        };
        | _ => fail(loc, "Expected expression as attribute payload")
    };