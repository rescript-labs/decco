open Migrate_parsetree;
open Ast_402;
open Ast_helper;

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