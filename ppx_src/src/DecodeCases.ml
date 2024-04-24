open Ppxlib
open Parsetree
open Ast_helper
open Utils
let generateErrorCase numArgs i _ =
  {
    pc_lhs =
      (((Array.init numArgs
           (fun which ->
              match which == i with
              | true ->
                  [%pat ? ((Belt.Result.Error
                    ((e : Decco.decodeError)))[@explicit_arity ])]
              | false -> [%pat ? _]))
          |> Array.to_list)
         |> (tupleOrSingleton Pat.tuple));
    pc_guard = None;
    pc_rhs =
      ([%expr
         ((Belt.Result.Error
             ({ e with path = (([%e indexConst i]) ^ e.path) }))
           [@explicit_arity ])])
  }