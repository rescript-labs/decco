open Ppxlib
open Parsetree
open Ast_helper
open Utils

let jsJsonTypeDecl = Typ.constr (lid "Js.Json.t") []

let buildRightHandSideOfEqualSignForCodecDeclarations (paramNames : label list)
    (codecGutsExpression : expression) (typeInfo : typeInfo) (isEncoder : bool)
    =
  (* If we're dealing with an encoder, we need to specify the exact type that
     will be fed to this function. If it's a decoder, we're always taking in
     JSON *)
  let incomingType =
    if isEncoder then typeNameAndParamsToTypeDeclaration typeInfo
    else jsJsonTypeDecl
  in
  let returnType =
    if isEncoder then jsJsonTypeDecl
    else
      Ast_helper.Typ.constr (lid "Belt.Result.t")
        [
          typeNameAndParamsToTypeDeclaration typeInfo;
          Utils.labelToCoreType "Decco.decodeError";
        ]
  in
  (* This is the node that specifies the arguments coming in to the function *)
  let basePattern =
    Ast_helper.Pat.constraint_
      (Ast_helper.Pat.var (mknoloc "value"))
      incomingType
  in
  let codecGutsWithReturnType =
    Ast_helper.Exp.constraint_ [%expr [%e codecGutsExpression] value] returnType
  in
  (* The base expression is what you'd think of as the "codec function". Takes json, returns a type, and vice-versa
     but below we handle parameterized types by wrapping the base expression in new functions for every parameter.
     In OCaml, this is the same concept as having a function with an argument for each parameter. But not in Rescript,
     so down at the bottom we wrap the whole darn thing in a special expression that tells Rescript to use the same
     arity as the number of incoming params, thus building an uncurried function. *)
  let baseExpression =
    Exp.fun_ Asttypes.Nolabel None basePattern codecGutsWithReturnType
  in
  let wholeCodecExpr =
    List.fold_right
      (fun s acc ->
        let pat = Pat.var (mknoloc s) in
        Exp.fun_ Asttypes.Nolabel None pat acc)
      paramNames baseExpression
  in
  let arity = List.length paramNames + 1 in
  (* Set an attribute with the arity matching the param count on the
     outermost invocation so that we generate a function that's uncurried,
     expecting all of its arguments at once. *)
  Utils.wrapFunctionExpressionForUncurrying ~arity wholeCodecExpr

(* This is where the value bindings get made for the codec functions
   but it isn't where the codec functions themselves are generated. Those
   get passed in. This is the outermost layer of the t_encode and t_decode functions *)
let generateCodecDecls (typeInfo : typeInfo) (encoder, decoder) =
  let encoderPat =
    Pat.var (mknoloc (typeInfo.typeName ^ Utils.encoderFuncSuffix))
  in
  let encoderParamNames =
    List.map (fun s -> encoderVarPrefix ^ s) typeInfo.typeParams
  in
  let decoderPat =
    Pat.var (mknoloc (typeInfo.typeName ^ Utils.decoderFuncSuffix))
  in
  let decoderParamNames =
    List.map (fun s -> decoderVarPrefix ^ s) typeInfo.typeParams
  in
  let encoderBindings =
    match encoder with
    | None -> []
    | Some encoder ->
      [
        Vb.mk
          ~attrs:[attrWarning [%expr "-39"]]
          encoderPat
          (buildRightHandSideOfEqualSignForCodecDeclarations encoderParamNames
             encoder typeInfo true);
      ]
  in
  let decoderBindings =
    match decoder with
    | None -> []
    | Some decoder ->
      [
        Vb.mk
          ~attrs:[attrWarning [%expr "-4"]; attrWarning [%expr "-39"]]
          decoderPat
          (buildRightHandSideOfEqualSignForCodecDeclarations decoderParamNames
             decoder typeInfo false);
      ]
  in
  [] @ encoderBindings @ decoderBindings

(* mapTypeDecl is where we know we're working with a type definition. We don't know
   whether it's a decco type yet though. We may end up doing nothing here. Or we may
   end up generating codec functions that get returned to the caller. *)
let mapTypeDecl decl =
  let {
    ptype_attributes;
    ptype_name = {txt = typeName};
    ptype_manifest;
    ptype_params;
    ptype_loc;
    ptype_kind;
  } =
    decl
  in
  let isUnboxed =
    match Utils.getAttributeByName ptype_attributes "unboxed" with
    | Ok (Some _) -> true
    | _ -> false
  in
  match makeEncodeDecodeFlagsFromDecoratorAttributes ptype_attributes with
  | Ok None -> []
  | Ok (Some encodeDecodeFlags) -> (
    let typeInfo = {typeName; typeParams = getParamNames ptype_params} in
    (* Here we call the code to generate the codecs and build their
       value bindings (the let t_decode = ... part). We have various different
       types to handle, so there's a switch. Most simple cases are covered in
       Codecs.generateCodecs, but there are some cases that get handled in their
       own modules. Probably for the sake of breaking up complex code.
       Why aren't those cases just handled in Codecs.generateCodecs? I'm not sure,
       I could probably find out by snooping around longer though. *)
    match (ptype_manifest, ptype_kind) with
    | None, Ptype_abstract ->
      fail ptype_loc "Can't generate codecs for unspecified type"
    | Some {ptyp_desc = Ptyp_variant (rowFields, _, _)}, Ptype_abstract ->
      let rowFieldsDec = List.map (fun row -> row.prf_desc) rowFields in
      generateCodecDecls typeInfo
        (Polyvariants.generateCodecs encodeDecodeFlags rowFieldsDec isUnboxed)
    | Some manifest, _ ->
      generateCodecDecls typeInfo
        (Codecs.generateCodecs encodeDecodeFlags manifest)
    | None, Ptype_variant decls ->
      generateCodecDecls typeInfo
        (Variants.generateCodecs encodeDecodeFlags decls isUnboxed)
    | None, Ptype_record decls ->
      generateCodecDecls typeInfo
        (Records.generateCodecs encodeDecodeFlags decls isUnboxed typeInfo)
    | _ -> fail ptype_loc "This type is not handled by decco")
  | Error s -> fail ptype_loc s

(* This is where we map over the AST, figure out if we need to generate
   values from a type or not, and stick those new values back into the
   generated code. *)
let mapStructureItem mapper ({pstr_desc} as structureItem) =
  match pstr_desc with
  | Pstr_type (recFlag, decls) ->
    (* If we've gotten into this branch, we're working with a type declaration
       and we want to potentially generate new values (codecs) based on
       the type. *)
    let valueBindings = decls |> List.map mapTypeDecl |> List.concat in
    let existingItem = [mapper#structure_item structureItem] in
    let newItems =
      match List.length valueBindings > 0 with
      | true -> [Str.value recFlag valueBindings]
      | false -> []
    in
    existingItem @ newItems
  | _ ->
    (* We've found some other structure item that isn't a type. Ignore it! *)
    [mapper#structure_item structureItem]