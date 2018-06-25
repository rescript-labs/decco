open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Ast_mapper;
open Parsetree;

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
        |> List.map(({ ptyp_desc, ptyp_loc }) => generateCodecs(ptyp_loc, ptyp_desc))
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

and generateCodecs = (loc, typeDesc) => {
    switch typeDesc {
        | Ptyp_any => fail(loc, "Can't generate codecs for `any` type")
        | Ptyp_alias(_, _) => fail(loc, "Can't generate codecs for aliased type")
        | Ptyp_arrow(_, _, _) => fail(loc, "Can't generate codecs for function type")
        | Ptyp_package(_) => fail(loc, "Can't generate codecs for module type")

        | Ptyp_var(s) => (
            makeIdentExpr(encoderVarPrefix ++ s),
            makeIdentExpr(decoderVarPrefix ++ s),
        )

        | Ptyp_constr(constr, typeArgs) => generateConstrCodecs(constr, typeArgs)

        | Ptyp_tuple(types) => {

            parameterizeCodecs(types,
                [%expr ()], [%expr Decco.array_from_json]
            )
        }

        | _ => fail(loc, "noo")
    };
};

let updateTypeDeclStructure = (typeName, paramNames, manifest) => {
    let (encoder, decoder) = generateCodecs(manifest.ptyp_loc, manifest.ptyp_desc);

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
            ptype_manifest, ptype_params, ptype_loc } = decl;

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
            switch ptype_manifest {
                | None => fail(ptype_loc, "Can't generate codecs for unspecified type")
                | Some(manifest) => updateTypeDeclStructure(typeName, paramNames, manifest)
            }
    };
};

let mapStructureItem = (mapper, { pstr_desc } as structureItem) =>
    switch pstr_desc {
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
