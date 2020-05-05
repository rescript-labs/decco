open Migrate_parsetree;
open Ast_406;
open Ppx_tools_406;
open Parsetree;
open Ast_helper;
open Utils;

let generateEncoder = (compositeEncoders) => {
    let arrExp = compositeEncoders
        |> List.mapi((i, e) => {
            let vExp = Exp.ident(Ast_convenience.lid("v" ++ string_of_int(i)));
            [%expr [%e e]([%e vExp])];
        })
        |> Exp.array;

    let deconstructorPattern = compositeEncoders
        |> List.mapi((i, _) => Pat.var(Location.mknoloc("v" ++ string_of_int(i))))
        |> Pat.tuple;

    [%expr ([%p deconstructorPattern]) => [%e arrExp] |> Js.Json.array];
};

let generateDecodeSuccessCase = (numArgs) => {
    pc_lhs:
        Array.init(numArgs, i =>
            Location.mknoloc("v" ++ string_of_int(i))
            |> Pat.var
            |> (p) => [%pat? Belt.Result.Ok([%p p])]
        )
        |> Array.to_list
        |> tupleOrSingleton(Pat.tuple),
    pc_guard: None,
    pc_rhs:
        Array.init(numArgs, i => makeIdentExpr("v" ++ string_of_int(i)))
        |> Array.to_list
        |> Exp.tuple
        |> (e) => [%expr Belt.Result.Ok([%e e])]
};

let generateDecodeSwitch = (compositeDecoders) => {
    let decodeExpr = compositeDecoders
        |> List.mapi((i, d) => {
            let ident = "v" ++ string_of_int(i)
                |> Ast_convenience.lid
                |> Exp.ident;
            [%expr [%e d]([%e ident])];
        })
        |> Exp.tuple;

    compositeDecoders
    |> List.mapi(DecodeCases.generateErrorCase(List.length(compositeDecoders)))
    |> List.append([generateDecodeSuccessCase(List.length(compositeDecoders))])
    |> Exp.match(decodeExpr);
};

let generateDecoder = (compositeDecoders) => {
    let matchArrPattern = compositeDecoders
        |> List.mapi((i, _) => Pat.var(Location.mknoloc("v" ++ string_of_int(i))))
        |> Pat.array;

    let matchPattern = [%pat? Js.Json.JSONArray([%p matchArrPattern])];

    let outerSwitch = Exp.match([%expr Js.Json.classify(json)], [
        Exp.case(matchPattern, generateDecodeSwitch(compositeDecoders)),
        Exp.case([%pat? Js.Json.JSONArray(_)], [%expr Decco.error({ path: Pervasives.__LOC__, message: "Incorrect cardinality", value: json })]),
        Exp.case([%pat? _], [%expr Decco.error({ path: Pervasives.__LOC__, message: "Not a tuple", value: json })])
    ]);

    [%expr (json) => [%e outerSwitch]];
};
