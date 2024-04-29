open Ppxlib
open Parsetree
open Ast_helper
open Utils
let rec parameterizeCodecs typeArgs encoderFunc decoderFunc encodeDecodeFlags =
  let subEncoders, subDecoders =
    typeArgs
    |> List.map (fun core_type -> generateCodecs encodeDecodeFlags core_type)
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

(* The optional expressions that are returned from this function should be codec functions themselves.
   Not bindings. The receiver will invoke them with the value when it decides to. *)
and generateCodecsFromTypeConstructor {doEncode; doDecode}
    {Location.txt = identifier; loc} =
  let open Longident in
  match identifier with
  | Lident "string" ->
    ( (match doEncode with
      | true -> Some [%expr Decco.stringToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.stringFromJson]
      | false -> None )
  | Lident "int" ->
    ( (match doEncode with
      | true -> Some [%expr Decco.intToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.intFromJson]
      | false -> None )
  | Lident "int64" ->
    ( (match doEncode with
      | true -> Some [%expr Decco.int64ToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.int64FromJson]
      | false -> None )
  | Lident "float" ->
    ( (match doEncode with
      | true -> Some [%expr Decco.floatToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.floatFromJson]
      | false -> None )
  | Lident "bool" ->
    ( (match doEncode with
      | true -> Some [%expr Decco.boolToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.boolFromJson]
      | false -> None )
  | Lident "unit" ->
    ( (match doEncode with
      | true -> Some [%expr Decco.unitToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.unitFromJson]
      | false -> None )
  | Lident "array" ->
    ( (match doEncode with
      | true -> Some [%expr Decco.arrayToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.arrayFromJson]
      | false -> None )
  | Lident "list" ->
    ( (match doEncode with
      | true -> Some [%expr Decco.listToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.listFromJson]
      | false -> None )
  | Lident "option" ->
    ( (match doEncode with
      | true -> Some [%expr Decco.optionToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.optionFromJson]
      | false -> None )
  | Ldot (Ldot (Lident "Belt", "Result"), "t") ->
    ( (match doEncode with
      | true -> Some [%expr Decco.resultToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.resultFromJson]
      | false -> None )
  | Ldot (Ldot (Lident "Js", "Dict"), "t") ->
    ( (match doEncode with
      | true -> Some [%expr Decco.dictToJson]
      | false -> None),
      match doDecode with
      | true -> Some [%expr Decco.dictFromJson]
      | false -> None )
  | Ldot (Ldot (Lident "Js", "Json"), "t") ->
    ( (match doEncode with
      | true ->
        Some
          (Utils.wrapFunctionExpressionForUncurrying ~arity:1
             [%expr fun v -> v])
      | false -> None),
      match doDecode with
      | true ->
        Some
          (Utils.wrapFunctionExpressionForUncurrying ~arity:1
             [%expr fun v -> Belt.Result.Ok v])
      | false -> None )
  | Lident s ->
    (* Lident is such an abstract name. This is when we're handling a reference to something
       that isn't some special syntactic construct. For example, in type blah = string, the
       'string' part is a Lident. Same thing if we had `type blah = user`. The `user` part
        would be a Lident. *)
    ( (match doEncode with
      | true -> Some (makeIdentExpr (s ^ Utils.encoderFuncSuffix))
      | false -> None),
      match doDecode with
      | true -> Some (makeIdentExpr (s ^ Utils.decoderFuncSuffix))
      | false -> None )
  | Ldot (left, right) ->
    ( (match doEncode with
      | true ->
        Some
          (Exp.ident (mknoloc (Ldot (left, right ^ Utils.encoderFuncSuffix))))
      | false -> None),
      match doDecode with
      | true ->
        Some
          (Exp.ident (mknoloc (Ldot (left, right ^ Utils.decoderFuncSuffix))))
      | false -> None )
  | Lapply (_, _) -> fail loc "Lapply syntax not yet handled by decco"

(* This gets called when a type declaration has a @decco.codec decorator with
   custom functions. *)
and generateCustomCodecs attribute {doEncode; doDecode} =
  let expr = Utils.getExpressionFromPayload attribute in
  ( (match doEncode with
    | true ->
      Some
        [%expr
          let e, _ = [%e expr] in
          e]
    | false -> None),
    match doDecode with
    | true ->
      Some
        [%expr
          let _, d = [%e expr] in
          d]
    | false -> None )

(* This is a recursive function that operates on core types to make generators. core types
   might not be what you think, like strings and ints. Core types as far as the AST is
   concerned are, I think, basic elements of the parse tree. So this is going to be called
   not only with type declarations like 'type foo = string', but also with smaller parts of
   that declaration, like just 'string' *)
and generateCodecs ({doEncode; doDecode} as encodeDecodeFlags)
    {ptyp_desc; ptyp_loc; ptyp_attributes} =
  match ptyp_desc with
  | Ptyp_any -> fail ptyp_loc "Can't generate codecs for `any` type"
  | Ptyp_arrow (_, _, _) ->
    fail ptyp_loc "Can't generate codecs for function type"
  | Ptyp_package _ -> fail ptyp_loc "Can't generate codecs for module type"
  | Ptyp_tuple types -> (
    let compositeCodecs = List.map (generateCodecs encodeDecodeFlags) types in
    ( (match doEncode with
      | true ->
        Some
          (compositeCodecs
          |> List.map (fun (e, _) -> BatOption.get e)
          |> Tuple.generateEncoder)
      | false -> None),
      match doDecode with
      | true ->
        Some
          (compositeCodecs
          |> List.map (fun (_, d) -> BatOption.get d)
          |> Tuple.generateDecoder)
      | false -> None ))
  | Ptyp_var s ->
    (* In this branch we're handling a type variable, like 'a in option<'a> *)
    ( (match doEncode with
      | true -> Some (makeIdentExpr (encoderVarPrefix ^ s))
      | false -> None),
      match doDecode with
      | true -> Some (makeIdentExpr (decoderVarPrefix ^ s))
      | false -> None )
  | Ptyp_constr (constr, typeArgs) -> (
    (* Here we're handling a type constructor. This might be a type constructor with
       a name, like `type blah = string`, or it might be a nameless type constructor,
       like `string`, or `pizza`. When you read "constructor" here, don't think
       of only a type definition, but think of any time a type is mentioned at all,
       syntactically, mentioning a type is "constructing" that type. *)
    let customCodec = getAttributeByName ptyp_attributes "decco.codec" in
    let encode, decode =
      match customCodec with
      (* Shortcut! We're handling a type where the user has specified their own
         codec functions. Just return their settings instead of generating more
         of our own. *)
      | Ok (Some attribute) -> generateCustomCodecs attribute encodeDecodeFlags
      (* Hey! 👉 This is the most common branch. We're going to go generate codecs here based
         on the type constructor we're handling 👈 *)
      | Ok None -> generateCodecsFromTypeConstructor encodeDecodeFlags constr
      (* Arg, we couldn't even see if there was a custom codec to handle because
         of some unexpected error. *)
      | Error s -> fail ptyp_loc s
    in
    match List.length typeArgs = 0 with
    | true ->
      (* We've got a simple type here with no parameters! Just return the functions
         generated above *)
      (encode, decode)
    | false ->
      (* Looks like there are some params for this type. Let's handle
         those now. *)
      parameterizeCodecs typeArgs encode decode encodeDecodeFlags)
  | _ -> fail ptyp_loc "This syntax is not yet handled by decco"