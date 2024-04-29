open Ppxlib
open Parsetree
open Ast_helper
open Codecs
open Utils

let buildRightHandSideOfEqualSignForCodecDeclarations (paramNames : label list)
    (codecGutsExpression : expression) (typeName : string) =
  let wholeCodecExpr =
    List.fold_right
      (fun s acc ->
        let pat = Pat.var (mknoloc s) in
        Exp.fun_ Asttypes.Nolabel None pat acc)
      paramNames
      [%expr
        fun (value : [%t Utils.labelToCoreType typeName]) ->
          [%e codecGutsExpression] value]
  in
  let arity = List.length paramNames + 1 in
  (* Set an attribute with the arity matching the param count on the
     outermost invocation so that we generate a function that's uncurried,
     expecting all of its arguments at once. *)
  Utils.wrapFunctionExpressionForUncurrying ~arity wholeCodecExpr

(* Now we're getting into the guts a bit. This is where the actual
   t_encode and t_decode functions get generated (optionally). *)
let generateCodecDecls typeName paramNames (encoder, decoder) =
  let encoderPat = Pat.var (mknoloc (typeName ^ Utils.encoderFuncSuffix)) in
  let encoderParamNames = List.map (fun s -> encoderVarPrefix ^ s) paramNames in
  let decoderPat = Pat.var (mknoloc (typeName ^ Utils.decoderFuncSuffix)) in
  let decoderParamNames = List.map (fun s -> decoderVarPrefix ^ s) paramNames in
  let encoderBindings =
    match encoder with
    | None -> []
    | Some encoder ->
      [
        Vb.mk
          ~attrs:[attrWarning [%expr "-39"]]
          encoderPat
          (buildRightHandSideOfEqualSignForCodecDeclarations encoderParamNames
             encoder typeName);
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
             decoder typeName);
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
    match (ptype_manifest, ptype_kind) with
    | None, Ptype_abstract ->
      fail ptype_loc "Can't generate codecs for unspecified type"
    | Some {ptyp_desc = Ptyp_variant (rowFields, _, _)}, Ptype_abstract ->
      let rowFieldsDec = List.map (fun row -> row.prf_desc) rowFields in
      generateCodecDecls typeName
        (getParamNames ptype_params)
        (Polyvariants.generateCodecs encodeDecodeFlags rowFieldsDec isUnboxed)
    | Some manifest, _ ->
      generateCodecDecls typeName
        (getParamNames ptype_params)
        (generateCodecs encodeDecodeFlags manifest)
    | None, Ptype_variant decls ->
      generateCodecDecls typeName
        (getParamNames ptype_params)
        (Variants.generateCodecs encodeDecodeFlags decls isUnboxed)
    | None, Ptype_record decls ->
      generateCodecDecls typeName
        (getParamNames ptype_params)
        (Records.generateCodecs encodeDecodeFlags decls isUnboxed)
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