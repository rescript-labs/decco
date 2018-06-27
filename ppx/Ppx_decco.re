open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Ast_mapper;
open Parsetree;
open Ast_helper;

type decl =
    | CoreType(core_type_desc)
    | Kind(type_kind);

let annotationName = "decco";
let encoderVarPrefix = "encoder_";
let decoderVarPrefix = "decoder_";

let fail = (loc, message) =>
    Location.error(~loc, message)
        |> (v) => Location.Error(v)
        |> raise;

let addParams = (paramNames, expr) =>
    List.fold_right((s, acc) => {
        let pat = Pat.var(Location.mknoloc(s));
        Exp.fun_("", None, pat, acc);
    }, paramNames, [%expr (v) => [%e expr](v)]);

let makeIdentExpr = (s) =>
    Longident.parse(s)
        |> Location.mknoloc
        |> Exp.ident;

let rec parameterizeCodecs = (typeArgs, encoderFunc, decoderFunc) => {
    let (subEncoders, subDecoders) = typeArgs
        |> List.map(({ ptyp_desc, ptyp_loc }) => generateCodecs(CoreType(ptyp_desc), ptyp_loc))
        |> List.split;

    (
        subEncoders
            |> List.map(e => ("", e))
            |> Exp.apply(encoderFunc),
        subDecoders
            |> List.map(e => ("", e))
            |> Exp.apply(decoderFunc)
    )
}

and generateConstrCodecs = ({ Location.txt: identifier, loc }, typeArgs) =>
    switch (Longident.flatten(identifier)) {
        | [ "string" ] => ([%expr Decco.string_to_json], [%expr Decco.string_from_json])
        | [ "int" ] => ([%expr Decco.int_to_json], [%expr Decco.int_from_json])
        | [ "float" ] => ([%expr Decco.float_to_json], [%expr Decco.float_from_json])
        | [ "bool" ] => ([%expr Decco.bool_to_json], [%expr Decco.bool_from_json])
        | [ "unit" ] => ([%expr Decco.unit_to_json], [%expr Decco.unit_from_json])

        | [ "array" ] =>
            parameterizeCodecs(typeArgs,
                [%expr Decco.array_to_json], [%expr Decco.array_from_json]
            )

        | [ "list" ] =>
            parameterizeCodecs(typeArgs,
                [%expr Decco.list_to_json], [%expr Decco.list_from_json]
            )

        | [ "option" ] =>
            parameterizeCodecs(typeArgs,
                [%expr Decco.option_to_json], [%expr Decco.option_from_json]
            )

        | _ => fail(loc, "not implemented")
    }

and generateVariantEncoderCase = ({ pcd_name: { txt: name }, pcd_args, pcd_loc }) => {
    let lhsVars = switch pcd_args {
        | [] => None
        | [_] => Some(Pat.var(Location.mknoloc("v0")))
        | _ => pcd_args
            |> List.mapi((i, _) =>
                Location.mkloc("v" ++ string_of_int(i), pcd_loc) |> Pat.var)
            |> Pat.tuple
            |> (v) => Some(v)
    };

    let constructorExpr = Exp.constant(Asttypes.Const_string(name, None));

    let rhsArray = pcd_args
        |> List.map(({ ptyp_desc }) => generateCodecs(CoreType(ptyp_desc), pcd_loc))
        |> List.map(((encoder, _)) => encoder) /* TODO: refactor */
        |> List.mapi((i, e) => Exp.apply(~loc=pcd_loc, e, [("", makeIdentExpr("v" ++ string_of_int(i)))]))
        |> List.append([[%expr Js.Json.string([%e constructorExpr])]])
        |> Exp.array;

    {
        pc_lhs: Pat.construct(Location.mknoloc(Longident.parse(name)), lhsVars),
        pc_guard: None,
        pc_rhs: [%expr Js.Json.array([%e rhsArray])]
    }
}

and indexConst = (i) =>
    Asttypes.Const_string("[" ++ string_of_int(i) ++ "]", None)
        |> Exp.constant

and generateVariantDecodeErrorCase = (numArgs, i, _) => {
    pc_lhs:
        Array.init(numArgs, which =>
            which === i ? [%pat? Error(e)] : [%pat? _]
        )
        |> Array.to_list
        |> (v) => numArgs > 1 ? Pat.tuple(v) : List.hd(v),
    pc_guard: None,
    pc_rhs: [%expr Error({ ...e, path: [%e indexConst(i)] ++ e.path })]
}

and generateVariantDecodeSuccessCase = (numArgs, constructorName) => {
    pc_lhs:
        Array.init(numArgs, i =>
            Location.mknoloc("v" ++ string_of_int(i))
                |> Pat.var
                |> (p) => Pat.construct(Location.mknoloc(Longident.parse("Ok")), Some(p))
        )
        |> Array.to_list
        |> (v) => numArgs > 1 ? Pat.tuple(v) : List.hd(v),
    pc_guard: None,
    pc_rhs:
        Array.init(numArgs, i => makeIdentExpr("v" ++ string_of_int(i)))
            |> Array.to_list
            |> ((v) => numArgs > 1 ? Exp.tuple(v) : List.hd(v))
            |> (v) => Some(v)
            |> Exp.construct(Location.mknoloc(Longident.parse(constructorName)))
            |> (v) => Some(v)
            |> Exp.construct(Location.mknoloc(Longident.parse("Ok")))
}

and generateVariantArgDecoder = (args, constructorName) => {
    let numArgs = List.length(args);
    args
        |> List.mapi(generateVariantDecodeErrorCase(numArgs))
        |> List.append([generateVariantDecodeSuccessCase(numArgs, constructorName)])
        |> Exp.match(args
            |> List.map(({ ptyp_desc, ptyp_loc }) => generateCodecs(CoreType(ptyp_desc), ptyp_loc))
            |> List.mapi((i, (_, decoder)) => Exp.apply(decoder, [ ("", {
                let idx = Asttypes.Const_int(i + 1) /* +1 because index 0 is the constructor */
                    |> Exp.constant;

                [%expr jsonArr[[%e idx]]];
            })]))
            |> (v) => numArgs > 1 ? Exp.tuple(v) : List.hd(v)
        );
}

and generateVariantDecoderCase = ({ pcd_name: { txt: name }, pcd_args }) => {
    let argLen = Asttypes.Const_int(List.length(pcd_args) + 1)
        |> Exp.constant;

    let decoded = switch(pcd_args) {
        | [] => {
            let ident = Longident.parse(name) |> Location.mknoloc;
            [%expr Ok([%e Exp.construct(ident, None)])]
        }
        | _ => generateVariantArgDecoder(pcd_args, name)
    };

    {
        pc_lhs: Asttypes.Const_string(name, None)
            |> Pat.constant
            |> (v) => Some(v)
            |> Pat.construct(Location.mknoloc(Longident.parse("Js.Json.JSONString"))),
        pc_guard: None,
        pc_rhs: [%expr
            (Js.Array.length(tagged) !== [%e argLen]) ?
                Decco.error("Invalid number of arguments to variant constructor", v)
            :
                [%e decoded]
        ]
    }
}

and generateVariantCodecs = (constrDecls) => {
    let encoder = List.map(generateVariantEncoderCase, constrDecls)
        |> Exp.match([%expr v])
        |> Exp.fun_("", None, [%pat? v]);

    let decoderDefaultCase = {
        pc_lhs: [%pat? _],
        pc_guard: None,
        pc_rhs: [%expr Decco.error("Invalid variant constructor", jsonArr[0])]
    };
    let decoderSwitch = List.map(generateVariantDecoderCase, constrDecls)
        |> (l) => l @ [ decoderDefaultCase ]
        |> Exp.match([%expr tagged[0]]);

    let decoder = [%expr  (v) =>
        switch (Js.Json.classify(v)) {
            | Js.Json.JSONArray(jsonArr) => {
                let tagged = Js.Array.map(Js.Json.classify, jsonArr);
                [%e decoderSwitch]
            }
            | _ => Decco.error("Not a variant", v)
        }
    ];

    (encoder, decoder);
}

and generateCodecs = (typeDesc, loc) => {
    switch typeDesc {
        | CoreType(Ptyp_any) => fail(loc, "Can't generate codecs for `any` type")
        | CoreType(Ptyp_alias(_, _)) => fail(loc, "Can't generate codecs for aliased type")
        | CoreType(Ptyp_arrow(_, _, _)) => fail(loc, "Can't generate codecs for function type")
        | CoreType(Ptyp_package(_)) => fail(loc, "Can't generate codecs for module type")

        | CoreType(Ptyp_var(s)) => (
            makeIdentExpr(encoderVarPrefix ++ s),
            makeIdentExpr(decoderVarPrefix ++ s),
        )

        | CoreType(Ptyp_constr(constr, typeArgs)) => generateConstrCodecs(constr, typeArgs)
        | Kind(Ptype_variant(constrDecls)) => generateVariantCodecs(constrDecls)

        | _ => fail(loc, "noo")
    };
};

let updateTypeDeclStructure = (typeName, paramNames, decl, loc) => {
    let (encoder, decoder) = generateCodecs(decl, loc);

    let encoderPat = Pat.var(Location.mknoloc(typeName ++ "__to_json"));
    let encoderParamNames = List.map(s => encoderVarPrefix ++ s, paramNames);

    let decoderPat = Pat.var(Location.mknoloc(typeName ++ "__from_json"));
    let decoderParamNames = List.map(s => decoderVarPrefix ++ s, paramNames);

    [
        [%str let [%p encoderPat] = [%e addParams(encoderParamNames, encoder)]],
        [%str let [%p decoderPat] = [%e addParams(decoderParamNames, decoder)]],
    ]
    |> List.concat;
};

let mapTypeDecl = (mapper, decl) => {
    let { ptype_attributes, ptype_name: { txt: typeName },
          ptype_manifest, ptype_params, ptype_loc, ptype_kind } = decl;

    let matchingAttributes = ptype_attributes
        |> List.filter((({ Location.txt }, _)) => txt == annotationName);

    let paramNames = ptype_params
        |> List.map((({ ptyp_desc, ptyp_loc }, _)) =>
            switch ptyp_desc {
                | Ptyp_var(s) => s
                | _ => fail(ptyp_loc, "Unhandled param type")
                    |> (v) => Location.Error(v)
                    |> raise
            }
        );

    switch matchingAttributes {
        | [] => []
        | _ =>
            switch (ptype_manifest, ptype_kind) {
                | (Some(manifest), _) => updateTypeDeclStructure(typeName, paramNames, CoreType(manifest.ptyp_desc), manifest.ptyp_loc)
                | (_, Ptype_variant(_)) => updateTypeDeclStructure(typeName, paramNames, Kind(ptype_kind), ptype_loc)
                | _ => fail(ptype_loc, "Can't generate codecs for unspecified type")
            }
    };
};

let mapStructureItem = (mapper, { pstr_desc } as structureItem) =>
    switch pstr_desc {
        /* | _ => {
            let constr = Type.constructor(~res=Typ.constr(Location.mknoloc(Longident.parse("res")), []), Location.mknoloc("Ha"));
            let kind = Ptype_variant([constr]);
            [[Type.mk(~kind, Location.mknoloc("yaya"))]
                |> Str.type_];
        } */

        | Pstr_type(decls) => {
            let generatedStructItems = decls
                |> List.map(mapTypeDecl(mapper))
                |> List.concat;

            [   mapper.structure_item(mapper, structureItem),
                ...generatedStructItems ];
        }

        | _ => [ mapper.structure_item(mapper, structureItem) ]
    };

let mapStructure = (mapper, structure) =>
    structure
        |> List.map(mapStructureItem(mapper))
        |> List.concat;

let mapper = (_, _) => { ...default_mapper, structure: mapStructure };

let () = Driver.register(~name="decco", Versions.ocaml_402, mapper);
