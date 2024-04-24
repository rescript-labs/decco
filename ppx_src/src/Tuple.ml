open Ppxlib
open Parsetree
open Ast_helper
open Utils
let generateEncoder compositeEncoders =
  let arrExp =
    (compositeEncoders |>
       (List.mapi
          (fun i ->
             fun e ->
               let vExp = Exp.ident (lid ("v" ^ (string_of_int i))) in
               [%expr ([%e e]) ([%e vExp])])))
      |> Exp.array in
  let deconstructorPattern =
    (compositeEncoders |>
       (List.mapi
          (fun i -> fun _ -> Pat.var (mknoloc ("v" ^ (string_of_int i))))))
      |> Pat.tuple in
  [%expr fun [%p deconstructorPattern] -> ([%e arrExp]) |> Js.Json.array]
let generateDecodeSuccessCase numArgs =
  {
    pc_lhs =
      (((Array.init numArgs
           (fun i ->
              ((mknoloc ("v" ^ (string_of_int i))) |> Pat.var) |>
                (fun p ->
                   [%pat ? ((Belt.Result.Ok ([%p p]))[@explicit_arity ])])))
          |> Array.to_list)
         |> (tupleOrSingleton Pat.tuple));
    pc_guard = None;
    pc_rhs =
      ((((Array.init numArgs
            (fun i -> makeIdentExpr ("v" ^ (string_of_int i))))
           |> Array.to_list)
          |> Exp.tuple)
         |>
         (fun e -> [%expr ((Belt.Result.Ok (([%e e])))[@explicit_arity ])]))
  }
let generateDecodeSwitch compositeDecoders =
  let decodeExpr =
    (compositeDecoders |>
       (List.mapi
          (fun i ->
             fun d ->
               let ident = makeIdentExpr ("v" ^ (string_of_int i)) in
               [%expr ([%e d]) ([%e ident])])))
      |> Exp.tuple in
  ((compositeDecoders |>
      (List.mapi
         (DecodeCases.generateErrorCase (List.length compositeDecoders))))
     |>
     (List.append [generateDecodeSuccessCase (List.length compositeDecoders)]))
    |> (Exp.match_ decodeExpr)
let generateDecoder compositeDecoders =
  let matchArrPattern =
    (compositeDecoders |>
       (List.mapi
          (fun i -> fun _ -> Pat.var (mknoloc ("v" ^ (string_of_int i))))))
      |> Pat.array in
  let matchPattern =
    [%pat ? ((Js.Json.JSONArray ([%p matchArrPattern]))[@explicit_arity ])] in
  let outerSwitch =
    Exp.match_ ([%expr Js.Json.classify json])
      [Exp.case matchPattern (generateDecodeSwitch compositeDecoders);
      Exp.case ([%pat ? Js.Json.JSONArray _])
        ([%expr Decco.error "Incorrect cardinality" json]);
      Exp.case ([%pat ? _]) ([%expr Decco.error "Not a tuple" json])] in
  [%expr fun json -> [%e outerSwitch]]