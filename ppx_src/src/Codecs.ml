open Ppxlib
open Parsetree
open Ast_helper
open Utils
let rec parameterizeCodecs typeArgs encoderFunc decoderFunc generatorSettings =
  let subEncoders, subDecoders =
    typeArgs
    |> List.map (fun core_type -> generateCodecs generatorSettings core_type)
    |> List.split
  in
  ( (match encoderFunc with
    | None -> None
    | ((Some encoderFunc) [@explicit_arity]) ->
      let uncurriedApplicationAttrs =
        [Attr.mk {txt = "res.partial"; loc} (PStr [])]
      in
      subEncoders
      |> List.map (fun e -> (Asttypes.Nolabel, BatOption.get e))
      |> Exp.apply ~attrs:uncurriedApplicationAttrs encoderFunc
      |> BatOption.some),
    match decoderFunc with
    | None -> None
    | ((Some decoderFunc) [@explicit_arity]) ->
      let uncurriedApplicationAttrs =
        [Attr.mk {txt = "res.partial"; loc} (PStr [])]
      in
      subDecoders
      |> List.map (fun e -> (Asttypes.Nolabel, BatOption.get e))
      |> Exp.apply ~attrs:uncurriedApplicationAttrs decoderFunc
      |> BatOption.some )

and generateConstrCodecs {doEncode; doDecode} {Location.txt = identifier; loc} =
  let open Longident in
  match identifier with
  | ((Lident "string") [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.stringToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.stringFromJson] [@explicit_arity]
      | false -> None )
  | ((Lident "int") [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.intToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.intFromJson] [@explicit_arity]
      | false -> None )
  | ((Lident "int64") [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.int64ToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.int64FromJson] [@explicit_arity]
      | false -> None )
  | ((Lident "float") [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.floatToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.floatFromJson] [@explicit_arity]
      | false -> None )
  | ((Lident "bool") [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.boolToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.boolFromJson] [@explicit_arity]
      | false -> None )
  | ((Lident "unit") [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.unitToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.unitFromJson] [@explicit_arity]
      | false -> None )
  | ((Lident "array") [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.arrayToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.arrayFromJson] [@explicit_arity]
      | false -> None )
  | ((Lident "list") [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.listToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.listFromJson] [@explicit_arity]
      | false -> None )
  | ((Lident "option") [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.optionToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.optionFromJson] [@explicit_arity]
      | false -> None )
  | ((Ldot
       ( ((Ldot (((Lident "Belt") [@explicit_arity]), "Result"))
         [@explicit_arity]),
         "t" ))
  [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.resultToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.resultFromJson] [@explicit_arity]
      | false -> None )
  | ((Ldot
       ( ((Ldot (((Lident "Js") [@explicit_arity]), "Dict")) [@explicit_arity]),
         "t" ))
  [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some [%expr Decco.dictToJson] [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.dictFromJson] [@explicit_arity]
      | false -> None )
  | ((Ldot
       ( ((Ldot (((Lident "Js") [@explicit_arity]), "Json")) [@explicit_arity]),
         "t" ))
  [@explicit_arity]) ->
    ( (match doEncode with
      | true ->
        Some
          (Utils.wrapFunctionExpressionForUncurrying ~arity:1
             [%expr fun v -> v])
        [@explicit_arity]
      | false -> None),
      match doDecode with
      | true ->
        Some
          (Utils.wrapFunctionExpressionForUncurrying ~arity:1
             [%expr fun v -> (Belt.Result.Ok v [@explicit_arity])])
        [@explicit_arity]
      | false -> None )
  | ((Lident s) [@explicit_arity]) ->
    ( (match doEncode with
      | true ->
        Some (makeIdentExpr (s ^ Utils.encoderFuncSuffix)) [@explicit_arity]
      | false -> None),
      match doDecode with
      | true ->
        Some (makeIdentExpr (s ^ Utils.decoderFuncSuffix)) [@explicit_arity]
      | false -> None )
  | ((Ldot (left, right)) [@explicit_arity]) ->
    ( (match doEncode with
      | true ->
        Some
          (Exp.ident
             (mknoloc
                (Ldot (left, right ^ Utils.encoderFuncSuffix) [@explicit_arity])))
        [@explicit_arity]
      | false -> None),
      match doDecode with
      | true ->
        Some
          (Exp.ident
             (mknoloc
                (Ldot (left, right ^ Utils.decoderFuncSuffix) [@explicit_arity])))
        [@explicit_arity]
      | false -> None )
  | ((Lapply (_, _)) [@explicit_arity]) ->
    fail loc "Lapply syntax not yet handled by decco"

and generateCodecs ({doEncode; doDecode} as generatorSettings)
    {ptyp_desc; ptyp_loc; ptyp_attributes} =
  match ptyp_desc with
  | Ptyp_any -> fail ptyp_loc "Can't generate codecs for `any` type"
  | ((Ptyp_arrow (_, _, _)) [@explicit_arity]) ->
    fail ptyp_loc "Can't generate codecs for function type"
  | Ptyp_package _ -> fail ptyp_loc "Can't generate codecs for module type"
  | ((Ptyp_tuple types) [@explicit_arity]) -> (
    let compositeCodecs = List.map (generateCodecs generatorSettings) types in
    ( (match doEncode with
      | true ->
        Some
          (compositeCodecs
          |> List.map (fun (e, _) -> BatOption.get e)
          |> Tuple.generateEncoder)
        [@explicit_arity]
      | false -> None),
      match doDecode with
      | true ->
        Some
          (compositeCodecs
          |> List.map (fun (_, d) -> BatOption.get d)
          |> Tuple.generateDecoder)
        [@explicit_arity]
      | false -> None ))
  | ((Ptyp_var s) [@explicit_arity]) ->
    ( (match doEncode with
      | true -> Some (makeIdentExpr (encoderVarPrefix ^ s)) [@explicit_arity]
      | false -> None),
      match doDecode with
      | true -> Some (makeIdentExpr (decoderVarPrefix ^ s)) [@explicit_arity]
      | false -> None )
  | ((Ptyp_constr (constr, typeArgs)) [@explicit_arity]) -> (
    let customCodec = getAttributeByName ptyp_attributes "decco.codec" in
    let encode, decode =
      match customCodec with
      | ((Ok None) [@explicit_arity]) ->
        generateConstrCodecs generatorSettings constr
      | ((Ok ((Some attribute) [@explicit_arity])) [@explicit_arity]) -> (
        let expr = getExpressionFromPayload attribute in
        ( (match doEncode with
          | true ->
            Some
              [%expr
                let e, _ = [%e expr] in
                e]
            [@explicit_arity]
          | false -> None),
          match doDecode with
          | true ->
            Some
              [%expr
                let _, d = [%e expr] in
                d]
            [@explicit_arity]
          | false -> None ))
      | ((Error s) [@explicit_arity]) -> fail ptyp_loc s
    in
    match List.length typeArgs = 0 with
    | true -> (encode, decode)
    | false -> parameterizeCodecs typeArgs encode decode generatorSettings)
  | _ -> fail ptyp_loc "This syntax is not yet handled by decco"