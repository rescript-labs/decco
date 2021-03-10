open Migrate_parsetree;
open Ast_406;
open Ppx_tools_406;
open Parsetree;
open Ast_helper;
open Utils;

let generateEncoderCase = (generatorSettings, unboxed, row) => {
    switch (row) {
      | Rtag({ txt: name, loc }, _attributes, _, coreTypes) => {
        let constructorExpr = Exp.constant(Pconst_string(name, None));
        let lhsVars = switch coreTypes {
            | [] => None
            | [_] => Some(Pat.var(Location.mknoloc("v0")))
            | _ =>
                coreTypes
                |> List.mapi((i, _) =>
                    Location.mkloc("v" ++ string_of_int(i), loc) |> Pat.var)
                |> Pat.tuple
                |> (v) => Some(v)
        };

        let rhsList = coreTypes
          |> List.map(Codecs.generateCodecs(generatorSettings))
          |> List.map(((encoder, _)) => BatOption.get(encoder)) /* TODO: refactor */
          |> List.mapi((i, e) =>
              Exp.apply(
                  ~loc, e,
                  [(Asttypes.Nolabel, makeIdentExpr("v" ++ string_of_int(i)))]
              )
          )
          |> List.append([[%expr Js.Json.string([%e constructorExpr])]]);

        {
          pc_lhs: Pat.variant(name, lhsVars),
          pc_guard: None,
          pc_rhs: unboxed
            ? List.tl(rhsList) |> List.hd
            : [%expr Js.Json.array([%e rhsList |> Exp.array])]
        }
      }
      /* We don't have enough information to generate a encoder */
      | Rinherit(coreType) => fail(coreType.ptyp_loc, "This syntax is not yet implemented by decco")
    };
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
        |> Exp.variant(constructorName)
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

let generateDecoderCase = (generatorSettings, row) => {
    switch (row) {
      | Rtag({ txt }, _, _, args) => {
        let argLen =
          Pconst_integer(string_of_int(List.length(args) + 1), None)
          |> Exp.constant;

        let decoded = switch(args) {
                | [] => {
                    let resultantExp = Exp.variant(txt, None);
                    [%expr Belt.Result.Ok([%e resultantExp])]
                }
                | _ => generateArgDecoder(generatorSettings, args, txt)
            };

        {
            pc_lhs: Pconst_string(txt, None)
                |> Pat.constant
                |> (v) => Some(v)
                |> Pat.construct(Ast_convenience.lid("Js.Json.JSONString")),
            pc_guard: None,
            pc_rhs: [%expr
                (Js.Array.length(tagged) !== [%e argLen]) ?
                    Decco.error("Invalid number of arguments to polyvariant constructor", v)
                :
                    [%e decoded]
            ]
        }
      }
      | Rinherit(coreType) => fail(coreType.ptyp_loc, "This syntax is not yet implemented by decco")
    };
};

let generateUnboxedDecode = (generatorSettings, row) => {
    switch (row) {
      | Rtag({ txt, loc }, _, _, args) => {
            switch args {
                | [a] => {
                    let (_, d) = Codecs.generateCodecs(generatorSettings, a);
                    switch d {
                        | Some(d) => {
                            let constructor = Exp.construct(
                                Ast_convenience.lid(txt), Some([%expr v])
                            );

                            Some([%expr (v) =>
                                [%e d](v)
                                -> Belt.Result.map(v => [%e constructor])
                            ])
                        }
                        | None => None
                    }
                }
                | _ => fail(loc, "Expected exactly one type argument")
            }
        }
      | Rinherit(coreType) => fail(coreType.ptyp_loc, "This syntax is not yet implemented by decco")
    }
};

let generateCodecs = ({ doEncode, doDecode } as generatorSettings, rowFields, unboxed) => {
    let encoder =
        doEncode ?
            List.map(generateEncoderCase(generatorSettings, unboxed), rowFields)
            |> Exp.match([%expr v])
            |> Exp.fun_(Asttypes.Nolabel, None, [%pat? v])
            |> BatOption.some
        : None;

     let decoderDefaultCase = {
        pc_lhs: [%pat? _],
        pc_guard: None,
        pc_rhs: [%expr Decco.error("Invalid polyvariant constructor", Belt.Array.getExn(jsonArr, 0))]
    };

    let decoder = !doDecode
        ? None
        : unboxed
            ? generateUnboxedDecode(generatorSettings, List.hd(rowFields))
            : {
              let decoderSwitch =
                  rowFields
                  |>  List.map(generateDecoderCase(generatorSettings))
                  |> (l) => l @ [ decoderDefaultCase ]
                  |> Exp.match([%expr Belt.Array.getExn(tagged, 0)]);

              Some([%expr (v) =>
                  switch (Js.Json.classify(v)) {
                      | Js.Json.JSONArray([||]) =>
                          Decco.error("Expected polyvariant, found empty array", v)

                      | Js.Json.JSONArray(jsonArr) => {
                          let tagged = Js.Array.map(Js.Json.classify, jsonArr);
                          [%e decoderSwitch]
                      }

                      | _ => Decco.error("Not a polyvariant", v)
                  }
              ]);
          };

    (encoder, decoder);
};
