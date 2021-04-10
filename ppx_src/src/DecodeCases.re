open Ppxlib;
open Parsetree;
open Ast_helper;
open Utils;

let generateErrorCase = (numArgs, i, _) => {
    pc_lhs:
        Array.init(numArgs, which =>
            which === i ? [%pat? Belt.Result.Error(e : Decco.decodeError)] : [%pat? _]
        )
        |> Array.to_list
        |> tupleOrSingleton(Pat.tuple),
    pc_guard: None,
    pc_rhs: [%expr Belt.Result.Error({ ...e, path: [%e indexConst(i)] ++ e.path })]
};
