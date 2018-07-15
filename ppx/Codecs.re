open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Ast_mapper;
open Parsetree;
open Ast_helper;
open Utils;

let rec parameterizeCodecs = (typeArgs, encoderFunc, decoderFunc) => {
    let (subEncoders, subDecoders) = typeArgs
        |> List.map(({ ptyp_desc, ptyp_loc }) => generateCodecs(ptyp_desc, ptyp_loc))
        |> List.split;

    (
        subEncoders
            |> List.map(e => ("", e))
            |> Exp.apply(encoderFunc),
        subDecoders
            |> List.map(e => ("", e))
            |> Exp.apply(decoderFunc)
    )
}

and generateConstrCodecs = ({ Location.txt: identifier, loc }, typeArgs) => {
    open Longident;

    let (encode, decode) = switch identifier {
        | Lident("string") => ([%expr Decco.string_to_json], [%expr Decco.string_from_json])
        | Lident("int") => ([%expr Decco.int_to_json], [%expr Decco.int_from_json])
        | Lident("float") => ([%expr Decco.float_to_json], [%expr Decco.float_from_json])
        | Lident("bool") => ([%expr Decco.bool_to_json], [%expr Decco.bool_from_json])
        | Lident("unit") => ([%expr Decco.unit_to_json], [%expr Decco.unit_from_json])
        | Lident("array") => ([%expr Decco.array_to_json], [%expr Decco.array_from_json])
        | Lident("list") => ([%expr Decco.list_to_json], [%expr Decco.list_from_json])
        | Lident("option") => ([%expr Decco.option_to_json], [%expr Decco.option_from_json])
        | Ldot(Ldot(Lident("Js"), "Json"), "t") => (
            [%expr (v) => v],
            [%expr (v) => Js.Result.Ok(v)]
        )
        | Lident(s) => (
            Exp.ident(Ast_convenience.lid(s ++ Utils.encoderFuncSuffix)),
            Exp.ident(Ast_convenience.lid(s ++ Utils.decoderFuncSuffix)),
        )
        | Ldot(left, right) => (
            Exp.ident(Location.mknoloc(Ldot(left, right ++ Utils.encoderFuncSuffix))),
            Exp.ident(Location.mknoloc(Ldot(left, right ++ Utils.decoderFuncSuffix))),
        )
        | Lapply(_, _) => fail(loc, "Lapply syntax not yet handled by decco")
    };

    List.length(typeArgs) == 0 ? (encode, decode)
    : parameterizeCodecs(typeArgs, encode, decode);
}

and generateCodecs = (typeDesc, loc) =>
    switch typeDesc {
        | Ptyp_any => fail(loc, "Can't generate codecs for `any` type")
        | Ptyp_arrow(_, _, _)=> fail(loc, "Can't generate codecs for function type")
        | Ptyp_package(_)=> fail(loc, "Can't generate codecs for module type")

        | Ptyp_var(s)=> (
            makeIdentExpr(encoderVarPrefix ++ s),
            makeIdentExpr(decoderVarPrefix ++ s),
        )

        | Ptyp_constr(constr, typeArgs)=> generateConstrCodecs(constr, typeArgs)

        | _ => fail(loc, "This syntax is not yet handled by decco")
    };