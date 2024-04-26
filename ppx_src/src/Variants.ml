open Ppxlib
open Parsetree
open Ast_helper
open Utils
let generateEncoderCase generatorSettings unboxed
    {pcd_name = {txt = name}; pcd_args; pcd_loc} =
  match pcd_args with
  | ((Pcstr_tuple args) [@explicit_arity]) ->
    let constructorExpr =
      Exp.constant (Pconst_string (name, Location.none, None) [@explicit_arity])
    in
    let lhsVars =
      match args with
      | [] -> None
      | _ :: [] -> Some (Pat.var (mknoloc "v0")) [@explicit_arity]
      | _ ->
        args
        |> List.mapi (fun i _ ->
               mkloc ("v" ^ string_of_int i) pcd_loc |> Pat.var)
        |> Pat.tuple
        |> fun v -> (Some v [@explicit_arity])
    in
    let rhsList =
      args
      |> List.map (Codecs.generateCodecs generatorSettings)
      |> List.map (fun (encoder, _) -> BatOption.get encoder)
      |> List.mapi (fun i e ->
             Exp.apply ~loc:pcd_loc e
               [(Asttypes.Nolabel, makeIdentExpr ("v" ^ string_of_int i))])
      |> List.append [[%expr Js.Json.string [%e constructorExpr]]]
    in
    {
      pc_lhs = Pat.construct (lid name) lhsVars;
      pc_guard = None;
      pc_rhs =
        (match unboxed with
        | true -> List.tl rhsList |> List.hd
        | false -> [%expr Js.Json.array [%e rhsList |> Exp.array]]);
    }
  | Pcstr_record _ -> fail pcd_loc "This syntax is not yet implemented by decco"

let generateDecodeSuccessCase numArgs constructorName =
  {
    pc_lhs =
      Array.init numArgs (fun i ->
          mknoloc ("v" ^ string_of_int i) |> Pat.var |> fun p ->
          [%pat? ((Belt.Result.Ok [%p p]) [@explicit_arity])])
      |> Array.to_list |> tupleOrSingleton Pat.tuple;
    pc_guard = None;
    pc_rhs =
      ( Array.init numArgs (fun i -> makeIdentExpr ("v" ^ string_of_int i))
      |> Array.to_list |> tupleOrSingleton Exp.tuple
      |> fun v ->
        (Some v [@explicit_arity]) |> Exp.construct (lid constructorName)
        |> fun e -> [%expr Belt.Result.Ok [%e e] [@explicit_arity]] );
  }

let generateArgDecoder generatorSettings args constructorName =
  let numArgs = List.length args in
  args
  |> List.mapi (DecodeCases.generateErrorCase numArgs)
  |> List.append [generateDecodeSuccessCase numArgs constructorName]
  |> Exp.match_
       (args
       |> List.map (Codecs.generateCodecs generatorSettings)
       |> List.mapi (fun i (_, decoder) ->
              Exp.apply (BatOption.get decoder)
                [
                  ( Asttypes.Nolabel,
                    let idx =
                      (Pconst_integer (string_of_int (i + 1), None)
                      [@explicit_arity])
                      |> Exp.constant
                    in
                    [%expr Belt.Array.getExn jsonArr [%e idx]] );
                ])
       |> tupleOrSingleton Exp.tuple)

let generateDecoderCase generatorSettings
    {pcd_name = {txt = name}; pcd_args; pcd_loc} =
  match pcd_args with
  | ((Pcstr_tuple args) [@explicit_arity]) ->
    let argLen =
      (Pconst_integer (string_of_int (List.length args + 1), None)
      [@explicit_arity])
      |> Exp.constant
    in
    let decoded =
      match args with
      | [] ->
        let ident = lid name in
        [%expr Belt.Result.Ok [%e Exp.construct ident None] [@explicit_arity]]
      | _ -> generateArgDecoder generatorSettings args name
    in
    {
      pc_lhs =
        ( (Pconst_string (name, Location.none, None) [@explicit_arity])
        |> Pat.constant
        |> fun v ->
          (Some v [@explicit_arity]) |> Pat.construct (lid "Js.Json.JSONString")
        );
      pc_guard = None;
      pc_rhs =
        [%expr
          match Js.Array.length tagged != [%e argLen] with
          | true ->
            Decco.error "Invalid number of arguments to variant constructor" v
          | false -> [%e decoded]];
    }
  | Pcstr_record _ -> fail pcd_loc "This syntax is not yet implemented by decco"

let generateUnboxedDecode generatorSettings
    {pcd_name = {txt = name}; pcd_args; pcd_loc} =
  match pcd_args with
  | ((Pcstr_tuple args) [@explicit_arity]) -> (
    match args with
    | a :: [] -> (
      let _, d = Codecs.generateCodecs generatorSettings a in
      match d with
      | ((Some d) [@explicit_arity]) ->
        let constructor =
          Exp.construct (lid name) (Some [%expr v] [@explicit_arity])
        in
        (Some
           [%expr
             fun v -> ([%e d] v |. Belt.Result.map) (fun v -> [%e constructor])]
        [@explicit_arity])
      | None -> None)
    | _ -> fail pcd_loc "Expected exactly one type argument")
  | Pcstr_record _ -> fail pcd_loc "This syntax is not yet implemented by decco"

let generateCodecs ({doEncode; doDecode} as generatorSettings) constrDecls
    unboxed =
  let encoder =
    match doEncode with
    | true ->
      List.map (generateEncoderCase generatorSettings unboxed) constrDecls
      |> Exp.match_ [%expr v]
      |> (fun e ->
           Utils.wrapFunctionExpressionForUncurrying ~arity:1
             (Exp.fun_ Asttypes.Nolabel None [%pat? v] e))
      |> BatOption.some
    | false -> None
  in
  let decoderDefaultCase =
    {
      pc_lhs = [%pat? _];
      pc_guard = None;
      pc_rhs =
        [%expr
          Decco.error "Invalid variant constructor"
            (Belt.Array.getExn jsonArr 0)];
    }
  in
  let decoder =
    match not doDecode with
    | true -> None
    | false -> (
      match unboxed with
      | true -> generateUnboxedDecode generatorSettings (List.hd constrDecls)
      | false ->
        let decoderSwitch =
          List.map (generateDecoderCase generatorSettings) constrDecls
          |> fun l ->
          l @ [decoderDefaultCase]
          |> Exp.match_ [%expr Belt.Array.getExn tagged 0]
        in
        (Some
           [%expr
             fun v ->
               match Js.Json.classify v with
               | ((Js.Json.JSONArray [||]) [@explicit_arity]) ->
                 Decco.error "Expected variant, found empty array" v
               | ((Js.Json.JSONArray jsonArr) [@explicit_arity]) ->
                 let tagged = Js.Array.map Js.Json.classify jsonArr in
                 [%e decoderSwitch]
               | _ -> Decco.error "Not a variant" v]
        [@explicit_arity]))
  in
  (encoder, decoder)