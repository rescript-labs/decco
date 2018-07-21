open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Parsetree;
open Ast_helper;
open Utils;

let generateEncoder = (compositeCodecs) => {
    let arrExp = compositeCodecs
        |> List.mapi((i, (e, _)) => {
            let vExp = Exp.ident(Ast_convenience.lid("v" ++ string_of_int(i)));
            [%expr [%e e]([%e vExp])];
        })
        |> Exp.array;

    let deconstructorPattern = compositeCodecs
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

let generateDecodeSwitch = (compositeCodecs) => {
    let decodeExpr = compositeCodecs
        |> List.mapi((i, (_, d)) => {
            let ident = "v" ++ string_of_int(i)
                |> Ast_convenience.lid
                |> Exp.ident;
            [%expr [%e d]([%e ident])];
        })
        |> Exp.tuple;

    List.mapi(DecodeCases.generateErrorCase(List.length(compositeCodecs)), compositeCodecs)
        |> List.append([generateDecodeSuccessCase(List.length(compositeCodecs))])
        |> Exp.match(decodeExpr);
};

let generateDecoder = (compositeCodecs) => {
    let matchArrPattern = compositeCodecs
        |> List.mapi((i, _) => Pat.var(Location.mknoloc("v" ++ string_of_int(i))))
        |> Pat.array;

    let matchPattern = [%pat? Js.Json.JSONArray([%p matchArrPattern])];

    let outerSwitch = Exp.match([%expr Js.Json.classify(json)], [
        Exp.case(matchPattern, generateDecodeSwitch(compositeCodecs)),
        Exp.case([%pat? Js.Json.JSONArray(_)], [%expr Decco.error("Incorrect cardinality", json)]),
        Exp.case([%pat? _], [%expr Decco.error("Not a tuple", json)])
    ]);

    [%expr (json) => [%e outerSwitch]];
};
