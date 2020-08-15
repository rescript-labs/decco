open Migrate_parsetree;
open Ast_406;
open Ppx_tools_406;
open Parsetree;
open Ast_helper;
open Utils;

let generateEncoderCase = (generatorSettings, unboxed, { pcd_name: { txt: name }, pcd_args, pcd_loc }) => {
    switch pcd_args {
        | Pcstr_tuple(args) => {
            let lhsVars = switch args {
                | [] => None
                | [_] => Some(Pat.var(Location.mknoloc("v0")))
                | _ =>
                    args
                    |> List.mapi((i, _) =>
                        Location.mkloc("v" ++ string_of_int(i), pcd_loc) |> Pat.var)
                    |> Pat.tuple
                    |> (v) => Some(v)
            };

            let constructorExpr = Exp.constant(Pconst_string(name, None));

            let rhsList = args
                |> List.map(Codecs.generateCodecs(generatorSettings))
                |> List.map(((encoder, _)) => BatOption.get(encoder)) /* TODO: refactor */
                |> List.mapi((i, e) =>
                    Exp.apply(
                        ~loc=pcd_loc, e,
                        [(Asttypes.Nolabel, makeIdentExpr("v" ++ string_of_int(i)))]
                    )
                )
                |> List.append([[%expr Js.Json.string([%e constructorExpr])]]);

            {
                pc_lhs: Pat.construct(Ast_convenience.lid(name), lhsVars),
                pc_guard: None,
                pc_rhs: unboxed
                  ? List.tl(rhsList) |> List.hd
                  : [%expr Js.Json.array([%e rhsList |> Exp.array])]
            }
        }
        | Pcstr_record(_) => fail(pcd_loc, "This syntax is not yet implemented by decco")
    }
};

let generateDecodeSuccessCase = (numArgs, constructorName) => {
    pc_lhs:
        Array.init(numArgs, i =>
            Location.mknoloc("v" ++ string_of_int(i))
            |> Pat.var
            |> (p) => [%pat? Belt.Result.Ok([%p p])]
        )
        |> Array.to_list
        |> tupleOrSingleton(Pat.tuple),
    pc_guard: None,
    pc_rhs:
        Array.init(numArgs, i => makeIdentExpr("v" ++ string_of_int(i)))
        |> Array.to_list
        |> tupleOrSingleton(Exp.tuple)
        |> (v) => Some(v)
        |> Exp.construct(Ast_convenience.lid(constructorName))
        |> (e) => [%expr Belt.Result.Ok([%e e])]
};

let generateArgDecoder = (generatorSettings, args, constructorName) => {
    let numArgs = List.length(args);
    args
    |> List.mapi(DecodeCases.generateErrorCase(numArgs))
    |> List.append([generateDecodeSuccessCase(numArgs, constructorName)])
    |> Exp.match(args
        |> List.map(Codecs.generateCodecs(generatorSettings))
        |> List.mapi((i, (_, decoder)) =>
            Exp.apply(BatOption.get(decoder), [(
                Asttypes.Nolabel,
                {
                    /* +1 because index 0 is the constructor */
                    let idx = Pconst_integer(string_of_int(i + 1), None)
                        |> Exp.constant;

                    [%expr Belt.Array.getExn(jsonArr, [%e idx])];
                }
            )])
        )
        |> tupleOrSingleton(Exp.tuple)
    );
};

let generateDecoderCase = (generatorSettings, { pcd_name: { txt: name }, pcd_args, pcd_loc }) => {
    switch pcd_args {
        | Pcstr_tuple(args) => {
            let argLen =
                Pconst_integer(string_of_int(List.length(args) + 1), None)
                |> Exp.constant;

            let decoded = switch(args) {
                | [] => {
                    let ident = Longident.parse(name) |> Location.mknoloc;
                    [%expr Belt.Result.Ok([%e Exp.construct(ident, None)])]
                }
                | _ => generateArgDecoder(generatorSettings, args, name)
            };

            {
                pc_lhs: Pconst_string(name, None)
                    |> Pat.constant
                    |> (v) => Some(v)
                    |> Pat.construct(Ast_convenience.lid("Js.Json.JSONString")),
                pc_guard: None,
                pc_rhs: [%expr
                    (Js.Array.length(tagged) !== [%e argLen]) ?
                        Decco.error("Invalid number of arguments to variant constructor", v)
                    :
                        [%e decoded]
                ]
            }
        }
        | Pcstr_record(_) => fail(pcd_loc, "This syntax is not yet implemented by decco")
    }
};

let generateUnboxedDecode = (generatorSettings, { pcd_name: { txt: name }, pcd_args, pcd_loc }) => {
    switch pcd_args {
        | Pcstr_tuple(args) => {
            switch args {
                | [a] => {
                    let (_, d) = Codecs.generateCodecs(generatorSettings, a);
                    switch d {
                        | Some(d) => {
                            let constructor = Exp.construct(
                                Ast_convenience.lid(name), Some([%expr v])
                            );

                            Some([%expr (v) =>
                                [%e d](v)
                                -> Belt.Result.map(v => [%e constructor])
                            ])
                        }
                        | None => None
                    }
                }
                | _ => fail(pcd_loc, "Expected exactly one type argument")
            }
        }
        | Pcstr_record(_) => fail(pcd_loc, "This syntax is not yet implemented by decco")
    }
};

let generateCodecs = ({ doEncode, doDecode } as generatorSettings, constrDecls, unboxed) => {
    let encoder =
        doEncode ?
            List.map(generateEncoderCase(generatorSettings, unboxed), constrDecls)
            |> Exp.match([%expr v])
            |> Exp.fun_(Asttypes.Nolabel, None, [%pat? v])
            |> BatOption.some
        : None;

    let decoderDefaultCase = {
        pc_lhs: [%pat? _],
        pc_guard: None,
        pc_rhs: [%expr Decco.error("Invalid variant constructor", Belt.Array.getExn(jsonArr, 0))]
    };

    let decoder = !doDecode
        ? None
        :
            unboxed
                ? generateUnboxedDecode(generatorSettings, List.hd(constrDecls))
                : {
                    let decoderSwitch =
                        List.map(generateDecoderCase(generatorSettings), constrDecls)
                        |> (l) => l @ [ decoderDefaultCase ]
                        |> Exp.match([%expr Belt.Array.getExn(tagged, 0)]);

                    Some([%expr (v) =>
                        switch (Js.Json.classify(v)) {
                            | Js.Json.JSONArray([||]) =>
                                Decco.error("Expected variant, found empty array", v)

                            | Js.Json.JSONArray(jsonArr) => {
                                let tagged = Js.Array.map(Js.Json.classify, jsonArr);
                                [%e decoderSwitch]
                            }

                            | _ => Decco.error("Not a variant", v)
                        }
                    ]);
                };

    (encoder, decoder);
};
