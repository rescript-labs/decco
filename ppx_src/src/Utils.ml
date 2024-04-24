open Ppxlib
open Parsetree
open Ast_helper
let annotationName = "decco"
let encoderFuncSuffix = "_encode"
let decoderFuncSuffix = "_decode"
let encoderVarPrefix = "encoder_"
let decoderVarPrefix = "decoder_"
let loc = !default_loc
let fail loc message = Location.raise_errorf ~loc "%s" message
let longidentParse = Longident.parse[@@ocaml.warning "-3"]
let mkloc txt loc = { Location.txt = txt; loc }
let mknoloc txt = mkloc txt Location.none
let lid ?(loc= Location.none)  s = mkloc (Longident.parse s) loc
let makeIdentExpr s = Exp.ident (mknoloc (longidentParse s))
let tupleOrSingleton tuple l =
  match (List.length l) > 1 with | true -> tuple l | false -> List.hd l
let getAttributeByName attributes name =
  let filtered =
    attributes |>
      (List.filter (fun { attr_name = { Location.txt = txt } } -> txt = name)) in
  match filtered with
  | [] -> ((Ok (None))[@explicit_arity ])
  | attribute::[] -> ((Ok (((Some (attribute))[@explicit_arity ])))
      [@explicit_arity ])
  | _ -> ((Error (("Too many occurrences of \"" ^ (name ^ "\" attribute"))))
      [@explicit_arity ])
type generatorSettings = {
  doEncode: bool ;
  doDecode: bool }
let getGeneratorSettingsFromAttributes attributes =
  match getAttributeByName attributes annotationName with
  | ((Ok (None))[@explicit_arity ]) ->
      (match ((getAttributeByName attributes (annotationName ^ ".decode")),
               (getAttributeByName attributes (annotationName ^ ".encode")))
       with
       | (((Ok (Some _))[@explicit_arity ]), ((Ok
          (Some _))[@explicit_arity ])) ->
           ((Ok
               (((Some ({ doEncode = true; doDecode = true }))
                 [@explicit_arity ])))
           [@explicit_arity ])
       | (((Ok (Some _))[@explicit_arity ]), ((Ok (None))[@explicit_arity ]))
           ->
           ((Ok
               (((Some ({ doEncode = false; doDecode = true }))
                 [@explicit_arity ])))
           [@explicit_arity ])
       | (((Ok (None))[@explicit_arity ]), ((Ok (Some _))[@explicit_arity ]))
           ->
           ((Ok
               (((Some ({ doEncode = true; doDecode = false }))
                 [@explicit_arity ])))
           [@explicit_arity ])
       | (((Ok (None))[@explicit_arity ]), ((Ok (None))[@explicit_arity ]))
           -> ((Ok (None))[@explicit_arity ])
       | ((Error _ as e), _) -> e
       | (_, (Error _ as e)) -> e)
  | ((Ok (Some _))[@explicit_arity ]) ->
      ((Ok
          (((Some ({ doEncode = true; doDecode = true }))[@explicit_arity ])))
      [@explicit_arity ])
  | Error _ as e -> e
let getExpressionFromPayload { attr_name = { loc }; attr_payload = payload }
  =
  match payload with
  | ((PStr ({ pstr_desc }::[]))[@explicit_arity ]) ->
      (match pstr_desc with
       | ((Pstr_eval (expr, _))[@explicit_arity ]) -> expr
       | _ -> fail loc "Expected expression as attribute payload")
  | _ -> fail loc "Expected expression as attribute payload"
let getParamNames params =
  params |>
    (List.map
       (fun ({ ptyp_desc; ptyp_loc }, _) ->
          match ptyp_desc with
          | ((Ptyp_var (s))[@explicit_arity ]) -> s
          | _ ->
              (fail ptyp_loc "Unhandled param type") |>
                ((fun v -> ((Location.Error (v))[@explicit_arity ]) |> raise))))
let indexConst i =
  ((Pconst_string (("[" ^ ((string_of_int i) ^ "]")), Location.none, None))
    [@explicit_arity ]) |> Exp.constant
let rec isIdentifierUsedInCoreType typeName { ptyp_desc; ptyp_loc } =
  match ptyp_desc with
  | ((Ptyp_arrow (_, _, _))[@explicit_arity ]) ->
      fail ptyp_loc "Can't generate codecs for function type"
  | Ptyp_any -> fail ptyp_loc "Can't generate codecs for `any` type"
  | Ptyp_package _ -> fail ptyp_loc "Can't generate codecs for module type"
  | ((Ptyp_variant (_, _, _))[@explicit_arity ]) ->
      fail ptyp_loc "Unexpected Ptyp_variant"
  | Ptyp_var _ -> false
  | ((Ptyp_tuple (childTypes))[@explicit_arity ]) ->
      List.exists (isIdentifierUsedInCoreType typeName) childTypes
  | ((Ptyp_constr ({ txt }, childTypes))[@explicit_arity ]) ->
      (match txt = ((Lident (typeName))[@explicit_arity ]) with
       | true -> true
       | false ->
           List.exists (isIdentifierUsedInCoreType typeName) childTypes)
  | _ -> fail ptyp_loc "This syntax is not yet handled by decco"
let attrWarning expr =
  {
    attr_name = (mkloc "ocaml.warning" loc);
    attr_payload =
      ((PStr
          ([{
              pstr_desc = ((Pstr_eval (expr, []))[@explicit_arity ]);
              pstr_loc = loc
            }]))[@explicit_arity ]);
    attr_loc = loc
  }