open Ppxlib
open Parsetree
open Ast_helper
open Utils

type parsedRecordFieldDeclaration = {
  name: string;
  key: expression;
  field: expression;
  codecs: expression option * expression option;
  default: expression option;
}

let makeArrayOfJsonFieldsFromParsedFieldDeclarations parsedFields =
  parsedFields
  |> List.map (fun {key; field; codecs = encoder, _} ->
         [%expr [%e key], [%e BatOption.get encoder] [%e field]])
  |> Exp.array

let wrapInSpreadEncoder parsedFields baseExpr =
  let spreadExpr =
    match List.find_opt (fun {name} -> name = "...") parsedFields with
    | Some {codecs = Some otherEncoder, _} ->
      (* We've encountered a spread operator here. At this point, we
         want to call the encode function for the name of the thing
         that's being spread, and then produce an expression that will
         merge another object over the encoded spread object.

         Make sure to use the text 'valueToEncode' here. It should match the value defined in
         generateEncoder below. There's a comment there about why we don't pass this name in
         as a parameter. *)
      let otherEncoderLident =
        [%expr [%e otherEncoder] (Obj.magic valueToEncode)]
      in
      Some [%expr Decco.unsafeMergeJsonObjectsCurried [%e otherEncoderLident]]
    | _ -> None
  in
  match spreadExpr with
  (* If we have a spread expression to apply, wrap the whole base encoder expression in
     the function that merges it with the result of the spread *)
  | Some spreadExpr -> [%expr [%e spreadExpr] [%e baseExpr]]
  | None ->
    (* If we're not handling a spread record, just return the base encoder expression *)
    baseExpr

let generateEncoder parsedFields unboxed (rootTypeNameOfRecord : label) =
  (* If we've got a record with a spread type in it, we'll need to omit the spread
     from the generated fields, and handle its encoding differently. *)
  let parsedFieldsWithoutSpread =
    List.filter (fun {name} -> name <> "...") parsedFields
  in
  let constrainedFunctionArgsPattern =
    (* Make sure you use the specific name 'valueToEncode' here, becuase it's also
       used above when calling the encoder for a spread. Instead of passing in a
       variable with the name, I'm writing the name directly in the quoted expression,
       because expression quotes don't support dropping strings in, and I'd have to
       do more construction of things by hand with Ast_helper. *)
    Ast_helper.Pat.constraint_
      [%pat? valueToEncode]
      (Utils.labelToCoreType rootTypeNameOfRecord)
  in
  match unboxed with
  | true ->
    (* In unboxed mode, we aren't going to handle spreading at all, since unboxeding
       is only supported on records with one field anyway. *)
    let {codecs; field} = List.hd parsedFieldsWithoutSpread in
    let e, _ = codecs in
    Exp.fun_ Asttypes.Nolabel None constrainedFunctionArgsPattern
      [%expr [%e BatOption.get e] [%e field]]
  | false ->
    [%expr
      Js.Json.object_
        (Js.Dict.fromArray
           [%e
             makeArrayOfJsonFieldsFromParsedFieldDeclarations
               parsedFieldsWithoutSpread])]
    |> wrapInSpreadEncoder parsedFields
    (* This is where the final encoder function is constructed. If
       you need to do something with the parameters, this is the place. *)
    |> Exp.fun_ Asttypes.Nolabel None constrainedFunctionArgsPattern

let generateDictGet {key; codecs = _, decoder; default} =
  let decoder = BatOption.get decoder in
  match default with
  | Some default ->
    [%expr
      ((Js.Dict.get dict [%e key] |. Belt.Option.map) [%e decoder]
      |. Belt.Option.getWithDefault)
        (Belt.Result.Ok [%e default])]
  | None ->
    [%expr
      (Js.Dict.get dict [%e key] |. Belt.Option.getWithDefault) Js.Json.null
      |> [%e decoder]]

let generateDictGets decls =
  decls |> List.map generateDictGet |> tupleOrSingleton Exp.tuple

let generateErrorCase {key} =
  {
    pc_lhs = [%pat? Belt.Result.Error (e : Decco.decodeError)];
    pc_guard = None;
    pc_rhs = [%expr Belt.Result.Error {e with path = "." ^ [%e key] ^ e.path}];
  }

let generateFinalRecordExpr allFieldDeclarations =
  allFieldDeclarations
  |> List.map (fun {name} -> (lid name, makeIdentExpr name))
  |> fun l -> [%expr Belt.Result.Ok [%e Exp.record l None]]

let generateSuccessCase {name} successExpr =
  {
    pc_lhs = (mknoloc name |> Pat.var |> fun p -> [%pat? Belt.Result.Ok [%p p]]);
    pc_guard = None;
    pc_rhs = successExpr;
  }

let rec generateNestedSwitchesRecurse allDecls remainingDecls =
  let current, successExpr =
    match remainingDecls with
    | [] -> failwith "Decco internal error: [] not expected"
    | last :: [] -> (last, generateFinalRecordExpr allDecls)
    | first :: tail -> (first, generateNestedSwitchesRecurse allDecls tail)
  in
  [generateErrorCase current]
  |> List.append [generateSuccessCase current successExpr]
  |> Exp.match_ (generateDictGet current)
[@@ocaml.doc
  " Recursively generates an expression containing nested switches, first\n\
  \ *  decoding the first record items, then (if successful) the second, etc. "]

let generateNestedSwitches decls = generateNestedSwitchesRecurse decls decls

let generateDecoder decls unboxed =
  let fieldDeclarationsWithoutSpread =
    List.filter (fun {name} -> name <> "...") decls
  in
  match unboxed with
  | true ->
    let {codecs; name} = List.hd fieldDeclarationsWithoutSpread in
    let _, d = codecs in
    let recordExpr =
      [(lid name, makeIdentExpr "v")] |> fun __x -> Exp.record __x None
    in
    [%expr
      fun v ->
        ([%e BatOption.get d] v |. Belt.Result.map) (fun v -> [%e recordExpr])]
  | false ->
    [%expr
      fun v ->
        match Js.Json.classify v with
        | Js.Json.JSONObject dict ->
          [%e generateNestedSwitches fieldDeclarationsWithoutSpread]
        | _ -> Decco.error "Not an object" v]

let parseRecordField encodeDecodeFlags (rootTypeNameOfRecord : label)
    {pld_name = {txt}; pld_loc; pld_type; pld_attributes} =
  let default =
    match getAttributeByName pld_attributes "decco.default" with
    | Ok (Some attribute) -> Some (getExpressionFromPayload attribute)
    | Ok None -> None
    | Error s -> fail pld_loc s
  in
  let key =
    match getAttributeByName pld_attributes "decco.key" with
    | Ok (Some attribute) -> getExpressionFromPayload attribute
    | Ok None -> Exp.constant (Pconst_string (txt, Location.none, None))
    | Error s -> fail pld_loc s
  in
  {
    name = txt;
    key;
    field = Exp.field [%expr valueToEncode] (lid txt);
    codecs = Codecs.generateCodecs encodeDecodeFlags pld_type;
    default;
  }

let generateCodecs ({doEncode; doDecode} as encodeDecodeFlags)
    recordFieldDeclarations unboxed (rootTypeNameOfRecord : label) =
  let parsedFieldDeclarations =
    List.map
      (parseRecordField encodeDecodeFlags rootTypeNameOfRecord)
      recordFieldDeclarations
  in
  ( (match doEncode with
    | true ->
      Some
        (generateEncoder parsedFieldDeclarations unboxed rootTypeNameOfRecord)
    | false -> None),
    match doDecode with
    | true -> Some (generateDecoder parsedFieldDeclarations unboxed)
    | false -> None )
