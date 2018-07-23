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
        | Lident("string") => ([%expr Decco.string_to_json], [%expr Decco.string_from_json])
        | Lident("int") => ([%expr Decco.int_to_json], [%expr Decco.int_from_json])
        | Lident("int64") => ([%expr Decco.int64_to_json], [%expr Decco.int64_from_json])
        | Lident("float") => ([%expr Decco.float_to_json], [%expr Decco.float_from_json])
        | Lident("bool") => ([%expr Decco.bool_to_json], [%expr Decco.bool_from_json])
        | Lident("unit") => ([%expr Decco.unit_to_json], [%expr Decco.unit_from_json])
        | Lident("array") => ([%expr Decco.array_to_json], [%expr Decco.array_from_json])
        | Lident("list") => ([%expr Decco.list_to_json], [%expr Decco.list_from_json])
        | Lident("option") => ([%expr Decco.option_to_json], [%expr Decco.option_from_json])
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
