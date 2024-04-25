open Ppxlib
open Parsetree
open Utils

let rec addEncoderParams paramNames resultType =
  match paramNames with
  | [] -> resultType
  | hd :: tl ->
    [%type: ([%t Ast_helper.Typ.var hd] -> Js.Json.t) -> [%t resultType]]
    |> addEncoderParams tl

let makeResultType valueType =
  [%type: ([%t valueType], Decco.decodeError) Belt.Result.t]

let rec addDecoderParams paramNames resultType =
  match paramNames with
  | [] -> resultType
  | hd :: tl ->
    let decoderParam =
      [%type: Js.Json.t -> [%t makeResultType (Ast_helper.Typ.var hd)]]
    in
    [%type: [%t decoderParam] -> [%t resultType]] |> addDecoderParams tl

let generateSigDecls {doEncode; doDecode} typeName paramNames =
  let encoderPat = typeName ^ Utils.encoderFuncSuffix in
  let decoderPat = typeName ^ Utils.decoderFuncSuffix in
  let valueType =
    paramNames
    |> List.map Ast_helper.Typ.var
    |> Ast_helper.Typ.constr (lid typeName)
  in
  let decls = [] in
  let encoderDecls =
    match doEncode with
    | true ->
      [
        [%type: [%t valueType] -> Js.Json.t]
        |> addEncoderParams (List.rev paramNames)
        |> Ast_helper.Val.mk (mknoloc encoderPat)
        |> Ast_helper.Sig.value;
      ]
    | false -> []
  in
  let decoderDecls =
    match doDecode with
    | true ->
      [
        [%type: Js.Json.t -> [%t makeResultType valueType]]
        |> addDecoderParams (List.rev paramNames)
        |> Ast_helper.Val.mk (mknoloc decoderPat)
        |> Ast_helper.Sig.value;
      ]
    | false -> []
  in
  List.concat [decls; encoderDecls; decoderDecls]

let mapTypeDecl decl =
  let {ptype_attributes; ptype_name = {txt = typeName}; ptype_params; ptype_loc}
      =
    decl
  in
  match getGeneratorSettingsFromAttributes ptype_attributes with
  | ((Error s) [@explicit_arity]) -> fail ptype_loc s
  | ((Ok None) [@explicit_arity]) -> []
  | ((Ok ((Some generatorSettings) [@explicit_arity])) [@explicit_arity]) ->
    generateSigDecls generatorSettings typeName (getParamNames ptype_params)

let mapSignatureItem mapper ({psig_desc} as signatureItem) =
  match psig_desc with
  | ((Psig_type (_, decls)) [@explicit_arity]) ->
    let generatedSigItems = decls |> List.map mapTypeDecl |> List.concat in
    mapper#signature_item signatureItem :: generatedSigItems
  | _ -> [mapper#signature_item signatureItem]