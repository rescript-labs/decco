open Ppxlib;
open Parsetree;
open Utils;

let rec addEncoderParams = (paramNames, resultType) =>
    switch paramNames {
        | [] => resultType
        | [ hd, ...tl ] =>
            [%type: ([%t Ast_helper.Typ.var(hd)] => Js.Json.t) => [%t resultType]]
                |> addEncoderParams(tl)
    };

let makeResultType = (valueType) =>
    [%type: Belt.Result.t([%t valueType], Decco.decodeError)];

let rec addDecoderParams = (paramNames, resultType) =>
    switch paramNames {
        | [] => resultType
        | [ hd, ...tl ] => {
            let decoderParam = [%type: (Js.Json.t => [%t makeResultType(Ast_helper.Typ.var(hd))])];
            [%type: [%t decoderParam] => [%t resultType]]
                |> addDecoderParams(tl)
        }
    };

let generateSigDecls = ({ doEncode, doDecode }, typeName, paramNames) => {
    let encoderPat = typeName ++ Utils.encoderFuncSuffix;
    let decoderPat = typeName ++ Utils.decoderFuncSuffix;

    let valueType = paramNames
        |> List.map(Ast_helper.Typ.var)
        |> Ast_helper.Typ.constr(lid(typeName));

    let decls = [];

    let decls =
        doEncode ?
            decls @
            [[%type: [%t valueType] => Js.Json.t]
                |> addEncoderParams(List.rev(paramNames))
                |> Ast_helper.Val.mk(mknoloc(encoderPat))
                |> Ast_helper.Sig.value]
        : decls;

    let decls =
        doDecode ?
            decls @
            [[%type: Js.Json.t => [%t makeResultType(valueType)]]
                |> addDecoderParams(List.rev(paramNames))
                |> Ast_helper.Val.mk(mknoloc(decoderPat))
                |> Ast_helper.Sig.value]
        : decls;

    decls;
};

let mapTypeDecl = (decl) => {
    let { ptype_attributes, ptype_name: { txt: typeName },
          ptype_params, ptype_loc } = decl;

    switch (getGeneratorSettingsFromAttributes(ptype_attributes)) {
        | Error(s) => fail(ptype_loc, s)
        | Ok(None) => []
        | Ok(Some(generatorSettings)) =>
            generateSigDecls(generatorSettings, typeName, getParamNames(ptype_params))
    };
};

let mapSignatureItem = (mapper, { psig_desc } as signatureItem) =>
    switch psig_desc {
        | Psig_type(_, decls) => {
            let generatedSigItems = decls
                |> List.map(mapTypeDecl)
                |> List.concat;

            [   mapper#signature_item(signatureItem),
                ...generatedSigItems ];
        }

        | _ => [ mapper#signature_item(signatureItem) ]
    };
