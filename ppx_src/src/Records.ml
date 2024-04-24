open Ppxlib
open Parsetree
open Ast_helper
open Utils
type parsedDecl =
  {
  name: string ;
  key: expression ;
  field: expression ;
  codecs: (expression option * expression option) ;
  default: expression option }
let generateEncoder decls unboxed =
  match unboxed with
  | true ->
      let { codecs; field } = List.hd decls in
      let (e, _) = codecs in
      [%expr (fun v -> ([%e BatOption.get e]) ([%e field]))]
  | false ->
      let arrExpr =
        (decls |>
           (List.map
              (fun { key; field; codecs = (encoder, _) } ->
                 [%expr
                   (([%e key]), (([%e BatOption.get encoder]) ([%e field])))])))
          |> Exp.array in
      ([%expr (([%e arrExpr]) |> Js.Dict.fromArray) |> Js.Json.object_]) |>
        (Exp.fun_ Asttypes.Nolabel None ([%pat ? v]))
let generateDictGet { key; codecs = (_, decoder); default } =
  let decoder = BatOption.get decoder in
  match default with
  | ((Some (default))[@explicit_arity ]) ->
      [%expr
        ((((Js.Dict.get dict ([%e key])) |. Belt.Option.map) ([%e decoder]))
           |. Belt.Option.getWithDefault) ((Belt.Result.Ok (([%e default])))
          [@explicit_arity ])]
  | None ->
      [%expr
        (((Js.Dict.get dict ([%e key])) |. Belt.Option.getWithDefault)
           Js.Json.null)
          |> ([%e decoder])]
let generateDictGets decls =
  (decls |> (List.map generateDictGet)) |> (tupleOrSingleton Exp.tuple)
let generateErrorCase { key } =
  {
    pc_lhs =
      ([%pat ? ((Belt.Result.Error
         ((e : Decco.decodeError)))[@explicit_arity ])]);
    pc_guard = None;
    pc_rhs =
      ([%expr
         ((Belt.Result.Error
             ({ e with path = ("." ^ (([%e key]) ^ e.path)) }))
           [@explicit_arity ])])
  }
let generateFinalRecordExpr decls =
  (decls |> (List.map (fun { name } -> ((lid name), (makeIdentExpr name)))))
    |>
    (fun l ->
       [%expr
         ((Belt.Result.Ok (([%e Exp.record l None])))[@explicit_arity ])])
let generateSuccessCase { name } successExpr =
  {
    pc_lhs =
      (((mknoloc name) |> Pat.var) |>
         (fun p -> [%pat ? ((Belt.Result.Ok ([%p p]))[@explicit_arity ])]));
    pc_guard = None;
    pc_rhs = successExpr
  }
let rec generateNestedSwitchesRecurse allDecls remainingDecls =
  let (current, successExpr) =
    match remainingDecls with
    | [] -> failwith "Decco internal error: [] not expected"
    | last::[] -> (last, (generateFinalRecordExpr allDecls))
    | first::tail -> (first, (generateNestedSwitchesRecurse allDecls tail)) in
  ([generateErrorCase current] |>
     (List.append [generateSuccessCase current successExpr]))
    |> (Exp.match_ (generateDictGet current))[@@ocaml.doc
                                               " Recursively generates an expression containing nested switches, first\n *  decoding the first record items, then (if successful) the second, etc. "]
let generateNestedSwitches decls = generateNestedSwitchesRecurse decls decls
let generateDecoder decls unboxed =
  match unboxed with
  | true ->
      let { codecs; name } = List.hd decls in
      let (_, d) = codecs in
      let recordExpr =
        [((lid name), (makeIdentExpr "v"))] |>
          (fun __x -> Exp.record __x None) in
      [%expr
        (fun v ->
           ((([%e BatOption.get d]) v) |. Belt.Result.map)
             (fun v -> [%e recordExpr]))]
  | false ->
      [%expr
        (fun v ->
           match Js.Json.classify v with
           | ((Js.Json.JSONObject (dict))[@explicit_arity ]) ->
               [%e generateNestedSwitches decls]
           | _ -> Decco.error "Not an object" v)]
let parseDecl generatorSettings
  { pld_name = { txt }; pld_loc; pld_type; pld_attributes } =
  let default =
    match getAttributeByName pld_attributes "decco.default" with
    | ((Ok (((Some (attribute))[@explicit_arity ])))[@explicit_arity ]) ->
        ((Some ((getExpressionFromPayload attribute)))[@explicit_arity ])
    | ((Ok (None))[@explicit_arity ]) -> None
    | ((Error (s))[@explicit_arity ]) -> fail pld_loc s in
  let key =
    match getAttributeByName pld_attributes "decco.key" with
    | ((Ok (((Some (attribute))[@explicit_arity ])))[@explicit_arity ]) ->
        getExpressionFromPayload attribute
    | ((Ok (None))[@explicit_arity ]) ->
        Exp.constant ((Pconst_string (txt, Location.none, None))
          [@explicit_arity ])
    | ((Error (s))[@explicit_arity ]) -> fail pld_loc s in
  {
    name = txt;
    key;
    field = (Exp.field ([%expr v]) (lid txt));
    codecs = (Codecs.generateCodecs generatorSettings pld_type);
    default
  }
let generateCodecs ({ doEncode; doDecode } as generatorSettings) decls
  unboxed =
  let parsedDecls = List.map (parseDecl generatorSettings) decls in
  ((match doEncode with
    | true -> ((Some ((generateEncoder parsedDecls unboxed)))
        [@explicit_arity ])
    | false -> None),
    (match doDecode with
     | true -> ((Some ((generateDecoder parsedDecls unboxed)))
         [@explicit_arity ])
     | false -> None))