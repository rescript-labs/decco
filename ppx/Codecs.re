open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Parsetree;
open Ast_helper;
open Utils;

let rec parameterizeCodecs = (typeArgs, encoderFunc, decoderFunc) => {
    let (subEncoders, subDecoders) = typeArgs
        |> List.map(core_type => generateCodecs(core_type))
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

and generateConstrCodecs = ({ Location.txt: identifier, loc }) => {
    open Longident;

    switch identifier {
        | Lident("string") => ([%expr Decco.stringToJson], [%expr Decco.stringFromJson])
        | Lident("int") => ([%expr Decco.intToJson], [%expr Decco.intFromJson])
        | Lident("int64") => ([%expr Decco.int64ToJson], [%expr Decco.int64FromJson])
        | Lident("float") => ([%expr Decco.floatToJson], [%expr Decco.floatFromJson])
        | Lident("bool") => ([%expr Decco.boolToJson], [%expr Decco.boolFromJson])
        | Lident("unit") => ([%expr Decco.unitToJson], [%expr Decco.unitFromJson])
        | Lident("array") => ([%expr Decco.arrayToJson], [%expr Decco.arrayFromJson])
        | Lident("list") => ([%expr Decco.listToJson], [%expr Decco.listFromJson])
        | Lident("option") => ([%expr Decco.optionToJson], [%expr Decco.optionFromJson])
        | Ldot(Ldot(Lident("Js"), "Json"), "t") => (
            [%expr (v) => v],
            [%expr (v) => Belt.Result.Ok(v)]
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
}

and generateCodecs = ({ ptyp_desc, ptyp_loc, ptyp_attributes }) => {
    switch ptyp_desc {
        | Ptyp_any => fail(ptyp_loc, "Can't generate codecs for `any` type")
        | Ptyp_arrow(_, _, _)=> fail(ptyp_loc, "Can't generate codecs for function type")
        | Ptyp_package(_)=> fail(ptyp_loc, "Can't generate codecs for module type")

        | Ptyp_tuple(types) => {
            let compositeCodecs = List.map(generateCodecs, types);
            (
                Tuple.generateEncoder(compositeCodecs),
                Tuple.generateDecoder(compositeCodecs)
            );
        }

        | Ptyp_var(s) => (
            makeIdentExpr(encoderVarPrefix ++ s),
            makeIdentExpr(decoderVarPrefix ++ s),
        )

        | Ptyp_constr(constr, typeArgs) => {
            let customCodec = getAttributeByName(ptyp_attributes, "decco.codec");
            let (encode, decode) = switch customCodec {
                | Ok(None) => generateConstrCodecs(constr)

                | Ok(Some(attribute)) => {
                    /* TODO: make this smarter to avoid duplicationg expr? */
                    let expr = getExpressionFromPayload(attribute);
                    (
                        [%expr { let (e, _) = [%e expr]; e }],
                        [%expr { let (_, d) = [%e expr]; d }],
                    )
                }

                | Error(s) => fail(ptyp_loc, s)
            };

            List.length(typeArgs) == 0 ? (encode, decode)
                : parameterizeCodecs(typeArgs, encode, decode);
        }

        | _ => fail(ptyp_loc, "This syntax is not yet handled by decco")
    };
};
