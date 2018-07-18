open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Ast_mapper;
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

let generateSigDecls = (typeName, paramNames) => {
    let encoderPat = typeName ++ Utils.encoderFuncSuffix;
    let decoderPat = typeName ++ Utils.decoderFuncSuffix;

    let valueType = paramNames
        |> List.map(Ast_helper.Typ.var)
        |> Ast_helper.Typ.constr(Ast_convenience.lid(typeName));

    [
        [%type: [%t valueType] => Js.Json.t]
            |> addEncoderParams(List.rev(paramNames))
            |> Ast_helper.Val.mk(Location.mknoloc(encoderPat))
            |> Ast_helper.Sig.value,

        [%type: Js.Json.t => [%t makeResultType(valueType)]]
            |> addDecoderParams(List.rev(paramNames))
            |> Ast_helper.Val.mk(Location.mknoloc(decoderPat))
            |> Ast_helper.Sig.value
    ];
};

let mapTypeDecl = (decl) => {
    let { ptype_attributes, ptype_name: { txt: typeName },
          ptype_params, ptype_loc } = decl;

    let matchingAttributes = getAttributeByName(ptype_attributes, annotationName);

    switch matchingAttributes {
        | Ok(None) => []
        | Ok(_) => generateSigDecls(typeName, getParamNames(ptype_params))
        | Error(s) => fail(ptype_loc, s)
    };
};

let mapSignatureItem = (mapper, { psig_desc } as signatureItem) =>
    switch psig_desc {
        | Psig_type(decls) => {
            let generatedSigItems = decls
                |> List.map(mapTypeDecl)
                |> List.concat;

            [   mapper.signature_item(mapper, signatureItem),
                ...generatedSigItems ];
        }

        | _ => [ mapper.signature_item(mapper, signatureItem) ]
    };