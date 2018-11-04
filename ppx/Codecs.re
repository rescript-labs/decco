open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Parsetree;
open Ast_helper;
open Utils;

let rec parameterizeCodecs = (typeArgs, encoderFunc, decoderFunc, generatorSettings) => {
    let (subEncoders, subDecoders) = typeArgs
        |> List.map(core_type => generateCodecs(generatorSettings, core_type))
        |> List.split;

    (
        switch encoderFunc {
        | None => None
        | Some(encoderFunc) =>
            subEncoders
            |> List.map(e => ("", BatOption.get(e)))
            |> Exp.apply(encoderFunc)
            |> BatOption.some
        },

        switch decoderFunc {
        | None => None
        | Some(decoderFunc) =>
            subDecoders
            |> List.map(e => ("", BatOption.get(e)))
            |> Exp.apply(decoderFunc)
            |> BatOption.some
        }
    )
}

and generateConstrCodecs = ({ doEncode, doDecode }, { Location.txt: identifier, loc }) => {
    open Longident;

    switch identifier {
        | Lident("string") => (
            doEncode ? Some([%expr Decco.stringToJson]) : None,
            doDecode ? Some([%expr Decco.stringFromJson]) : None
        )
        | Lident("int") => (
            doEncode ? Some([%expr Decco.intToJson]) : None,
            doDecode ? Some([%expr Decco.intFromJson]) : None
        )
        | Lident("int64") => (
            doEncode ? Some([%expr Decco.int64ToJson]) : None,
            doDecode ? Some([%expr Decco.int64FromJson]) : None
        )
        | Lident("float") => (
            doEncode ? Some([%expr Decco.floatToJson]) : None,
            doDecode ? Some([%expr Decco.floatFromJson]) : None
        )
        | Lident("bool") => (
            doEncode ? Some([%expr Decco.boolToJson]) : None,
            doDecode ? Some([%expr Decco.boolFromJson]) : None
        )
        | Lident("unit") => (
            doEncode ? Some([%expr Decco.unitToJson]) : None,
            doDecode ? Some([%expr Decco.unitFromJson]) : None
        )
        | Lident("array") => (
            doEncode ? Some([%expr Decco.arrayToJson]) : None,
            doDecode ? Some([%expr Decco.arrayFromJson]) : None
        )
        | Lident("list") => (
            doEncode ? Some([%expr Decco.listToJson]) : None,
            doDecode ? Some([%expr Decco.listFromJson]) : None
        )
        | Lident("option") => (
            doEncode ? Some([%expr Decco.optionToJson]) : None,
            doDecode ? Some([%expr Decco.optionFromJson]) : None
        )
        | Ldot(Ldot(Lident("Js"), "Json"), "t") => (
            doEncode ? Some([%expr (v) => v]) : None,
            doDecode ? Some([%expr (v) => Belt.Result.Ok(v)]) : None,
        )
        | Lident(s) => (
            doEncode ? Some(Exp.ident(Ast_convenience.lid(s ++ Utils.encoderFuncSuffix))) : None,
            doDecode ? Some(Exp.ident(Ast_convenience.lid(s ++ Utils.decoderFuncSuffix))) : None,
        )
        | Ldot(left, right) => (
            doEncode ? Some(Exp.ident(Location.mknoloc(Ldot(left, right ++ Utils.encoderFuncSuffix)))) : None,
            doDecode ? Some(Exp.ident(Location.mknoloc(Ldot(left, right ++ Utils.decoderFuncSuffix)))) : None,
        )
        | Lapply(_, _) => fail(loc, "Lapply syntax not yet handled by decco")
    };
}

and generateCodecs = (
    { doEncode, doDecode } as generatorSettings,
    { ptyp_desc, ptyp_loc, ptyp_attributes }
)
=> {
    switch ptyp_desc {
        | Ptyp_any => fail(ptyp_loc, "Can't generate codecs for `any` type")
        | Ptyp_arrow(_, _, _)=> fail(ptyp_loc, "Can't generate codecs for function type")
        | Ptyp_package(_)=> fail(ptyp_loc, "Can't generate codecs for module type")

        | Ptyp_tuple(types) => {
            let compositeCodecs = List.map(generateCodecs(generatorSettings), types);
            (
                doEncode ? Some(
                    compositeCodecs
                    |> List.map(((e, _)) => BatOption.get(e))
                    |> Tuple.generateEncoder)
                : None,
                doDecode ? Some(
                    compositeCodecs
                    |> List.map(((_, d)) => BatOption.get(d))
                    |> Tuple.generateDecoder)
                : None
            );
        }

        | Ptyp_var(s) => (
            doEncode ? Some(makeIdentExpr(encoderVarPrefix ++ s)) : None,
            doDecode ? Some(makeIdentExpr(decoderVarPrefix ++ s)) : None,
        )

        | Ptyp_constr(constr, typeArgs) => {
            let customCodec = getAttributeByName(ptyp_attributes, "decco.codec");
            let (encode, decode) = switch customCodec {
                | Ok(None) => generateConstrCodecs(generatorSettings, constr)

                | Ok(Some(attribute)) => {
                    /* TODO: make this smarter to avoid duplicationg expr? */
                    let expr = getExpressionFromPayload(attribute);
                    (
                        doEncode ? Some([%expr { let (e, _) = [%e expr]; e }]) : None,
                        doDecode ? Some([%expr { let (_, d) = [%e expr]; d }]) : None,
                    )
                }

                | Error(s) => fail(ptyp_loc, s)
            };

            List.length(typeArgs) == 0 ?
                (encode, decode)
            :
                parameterizeCodecs(typeArgs, encode, decode, generatorSettings);
        }

        | _ => fail(ptyp_loc, "This syntax is not yet handled by decco")
    };
};
