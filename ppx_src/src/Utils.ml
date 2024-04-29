open Ppxlib
open Parsetree
open Ast_helper

let decoratorLabel = "decco"
let decoratorDecodeLabel = decoratorLabel ^ ".decode"
let decoratorEncodeLabel = decoratorLabel ^ ".encode"
let encoderFuncSuffix = "_encode"
let decoderFuncSuffix = "_decode"
let encoderVarPrefix = "encoder_"
let decoderVarPrefix = "decoder_"

let loc = !default_loc

let fail loc message = Location.raise_errorf ~loc "%s" message

let longidentParse = Longident.parse [@@ocaml.warning "-3"]

let mkloc txt loc = {Location.txt; loc}

let mknoloc txt = mkloc txt Location.none

let lid ?(loc = Location.none) s = mkloc (Longident.parse s) loc

let makeIdentExpr s = Exp.ident (mknoloc (longidentParse s))

let tupleOrSingleton tuple l =
  match List.length l > 1 with
  | true -> tuple l
  | false -> List.hd l

let getAttributeByName attributes name =
  let filtered =
    attributes |> List.filter (fun {attr_name = {Location.txt}} -> txt = name)
  in
  match filtered with
  | [] -> Ok None [@explicit_arity]
  | attribute :: [] -> Ok (Some attribute [@explicit_arity]) [@explicit_arity]
  | _ ->
    Error ("Too many occurrences of \"" ^ name ^ "\" attribute")
    [@explicit_arity]

type encodeDecodeFlags = {doEncode: bool; doDecode: bool}

let makeEncodeDecodeFlagsFromDecoratorAttributes attributes =
  match getAttributeByName attributes decoratorLabel with
  | Ok None -> (
    (* This is the case where there's no @decco decorator found. We'll go ahead
       and check for encode/decode-specific versions instead *)
    match
      ( getAttributeByName attributes decoratorDecodeLabel,
        getAttributeByName attributes decoratorEncodeLabel )
    with
    | Ok (Some _), Ok (Some _) -> Ok (Some {doEncode = true; doDecode = true})
    | Ok (Some _), Ok None -> Ok (Some {doEncode = false; doDecode = true})
    | Ok None, Ok (Some _) -> Ok (Some {doEncode = true; doDecode = false})
    | Ok None, Ok None -> Ok None
    | (Error _ as e), _ -> e
    | _, (Error _ as e) -> e)
  | Ok (Some _) ->
    (* This is the case where the @decco decorator was found, which means we generate both *)
    Ok (Some {doEncode = true; doDecode = true})
  | Error _ as e -> e

let getExpressionFromPayload {attr_name = {loc}; attr_payload = payload} =
  match payload with
  | ((PStr ({pstr_desc} :: [])) [@explicit_arity]) -> (
    match pstr_desc with
    | ((Pstr_eval (expr, _)) [@explicit_arity]) -> expr
    | _ -> fail loc "Expected expression as attribute payload")
  | _ -> fail loc "Expected expression as attribute payload"

let getParamNames params =
  params
  |> List.map (fun ({ptyp_desc; ptyp_loc}, _) ->
         match ptyp_desc with
         | ((Ptyp_var s) [@explicit_arity]) -> s
         | _ ->
           fail ptyp_loc "Unhandled param type" |> fun v ->
           (Location.Error v [@explicit_arity]) |> raise)

let indexConst i =
  (Pconst_string ("[" ^ string_of_int i ^ "]", Location.none, None)
  [@explicit_arity])
  |> Exp.constant

let rec isIdentifierUsedInCoreType typeName {ptyp_desc; ptyp_loc} =
  match ptyp_desc with
  | ((Ptyp_arrow (_, _, _)) [@explicit_arity]) ->
    fail ptyp_loc "Can't generate codecs for function type"
  | Ptyp_any -> fail ptyp_loc "Can't generate codecs for `any` type"
  | Ptyp_package _ -> fail ptyp_loc "Can't generate codecs for module type"
  | ((Ptyp_variant (_, _, _)) [@explicit_arity]) ->
    fail ptyp_loc "Unexpected Ptyp_variant"
  | Ptyp_var _ -> false
  | ((Ptyp_tuple childTypes) [@explicit_arity]) ->
    List.exists (isIdentifierUsedInCoreType typeName) childTypes
  | ((Ptyp_constr ({txt}, childTypes)) [@explicit_arity]) -> (
    match txt = (Lident typeName [@explicit_arity]) with
    | true -> true
    | false -> List.exists (isIdentifierUsedInCoreType typeName) childTypes)
  | _ -> fail ptyp_loc "This syntax is not yet handled by decco"

let attrWarning expr =
  {
    attr_name = mkloc "ocaml.warning" loc;
    attr_payload =
      PStr
        [{pstr_desc = Pstr_eval (expr, []) [@explicit_arity]; pstr_loc = loc}]
      [@explicit_arity];
    attr_loc = loc;
  }

(* The following function comes from https://github.com/green-labs/ppx_spice/pull/49/files#diff-25e55eeac0911adb8041a5ee5c0a5fb5291bc174eea8711c3694c51bf6a219aaR127
   And are also under the MIT license, Copyright (c) 2021 Greenlabs *)
let wrapFunctionExpressionForUncurrying ?(loc = Location.none) ~arity e =
  let attr_arity =
    Attr.mk {txt = "res.arity"; loc}
      (PStr [Str.eval (Exp.constant (Const.int arity))])
  in
  Exp.construct ~attrs:[attr_arity] {txt = Lident "Function$"; loc} (Some e)
(* End function from Spice  *)

let wrapFunctionTypeSignatureForUncurrying ?(loc = Location.none) ~arity
    typeExpression =
  let arityType : core_type =
    Ast_helper.Typ.variant
      [Ast_helper.Rf.tag {txt = "Has_arity" ^ string_of_int arity; loc} true []]
      Closed None
  in
  Ast_helper.Typ.constr (lid "function$") [typeExpression; arityType]

let print_strings strings =
  let formatted = String.concat "; " strings in
  Printf.printf "[%s]\n" formatted

let labelToCoreType label = Ast_helper.Typ.constr (lid label) []
