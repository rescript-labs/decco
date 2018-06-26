open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Ast_mapper;
open Parsetree;

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
        let pat = Ast_helper.Pat.var(Location.mknoloc(s));
        Ast_helper.Exp.fun_("", None, pat, acc);
    }, paramNames, [%expr (v) => [%e expr](v)]);

let makeIdentExpr = (s) =>
    Longident.parse(s)
        |> Location.mknoloc
        |> Ast_helper.Exp.ident;

let rec parameterizeCodecs = (typeArgs, encoderFunc, decoderFunc) => {
    let (subEncoders, subDecoders) = typeArgs
        |> List.map(({ ptyp_desc, ptyp_loc }) => generateCodecs(CoreType(ptyp_desc), ptyp_loc))
        |> List.split;

    (
        subEncoders
            |> List.map(e => ("", e))
            |> Ast_helper.Exp.apply(encoderFunc),
        subDecoders
            |> List.map(e => ("", e))
            |> Ast_helper.Exp.apply(decoderFunc)
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

and generateVariantEncoderCase = ({ pcd_name: { txt: name } }) =>
    {
        pc_lhs: Ast_helper.Pat.construct(Location.mknoloc(Longident.parse(name)), None),
        pc_guard: None,
        pc_rhs: [%expr -1]
    }

and generateVariantCodecs = (constrDecls) =>
    (
        Ast_helper.Exp.match([%expr v], List.map(generateVariantEncoderCase, constrDecls)),
        [%expr failwith("unimplemented")]
    )

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

    let encoderPat = Ast_helper.Pat.var(Location.mknoloc(typeName ++ "__to_json"));
    let encoderParamNames = List.map(s => encoderVarPrefix ++ s, paramNames);

    let decoderPat = Ast_helper.Pat.var(Location.mknoloc(typeName ++ "__from_json"));
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
            let constr = Ast_helper.Type.constructor(~res=Ast_helper.Typ.constr(Location.mknoloc(Longident.parse("res")), []), Location.mknoloc("Ha"));
            let kind = Ptype_variant([constr]);
            [[Ast_helper.Type.mk(~kind, Location.mknoloc("yaya"))]
                |> Ast_helper.Str.type_];
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
