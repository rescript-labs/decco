open Migrate_parsetree;
open Ast_402;
open Ast_mapper;
open Parsetree;
open Ast_helper;
open Codecs;
open Utils;

type decl =
    | CoreType(core_type_desc)
    | Kind(type_kind);

let annotationName = "decco";

let addParams = (paramNames, expr) =>
    List.fold_right((s, acc) => {
        let pat = Pat.var(Location.mknoloc(s));
        Exp.fun_("", None, pat, acc);
    }, paramNames, [%expr (v) => [%e expr](v)]);

let updateTypeDeclStructure = (typeName, paramNames, (encoder, decoder)) => {
    let encoderPat = Pat.var(Location.mknoloc(typeName ++ Utils.encoderFuncSuffix));
    let encoderParamNames = List.map(s => encoderVarPrefix ++ s, paramNames);

    let decoderPat = Pat.var(Location.mknoloc(typeName ++ Utils.decoderFuncSuffix));
    let decoderParamNames = List.map(s => decoderVarPrefix ++ s, paramNames);

    [
        [%str let [%p encoderPat] = [%e addParams(encoderParamNames, encoder)]],
        [%str let [%p decoderPat] = [%e addParams(decoderParamNames, decoder)]],
    ]
    |> List.concat;
};

let mapTypeDecl = (decl) => {
    let { ptype_attributes, ptype_name: { txt: typeName },
          ptype_manifest, ptype_params, ptype_loc, ptype_kind } = decl;

    let matchingAttributes = getAttributeByName(ptype_attributes, annotationName);

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
        | Ok(None) => []
        | Ok(_) => switch (ptype_manifest, ptype_kind) {
            | (None, Ptype_abstract) => fail(ptype_loc, "Can't generate codecs for unspecified type")

            | (Some(manifest), _) => updateTypeDeclStructure(typeName, paramNames, generateCodecs(manifest))

            | (None, Ptype_variant(decls)) => updateTypeDeclStructure(typeName, paramNames,
                Variants.generateCodecs(decls)
            )
            | (None, Ptype_record(decls)) => updateTypeDeclStructure(typeName, paramNames,
                Records.generateCodecs(decls)
            )
            | _ => fail(ptype_loc, "This syntax is not handled by decco")
        }
        | Error(s) => fail(ptype_loc, s)
    };
};

let mapStructureItem = (mapper, { pstr_desc } as structureItem) =>
    switch pstr_desc {
        /* | _ => {
            let constr = Type.constructor(~res=Typ.constr(Ast_convenience.lid("res"), []), Location.mknoloc("Ha"));
            let kind = Parsetree.Ptype_open;
            [[Type.mk(~kind, Location.mknoloc("yaya"))]
                |> Str.type_];
        } */

        | Pstr_type(decls) => {
            let generatedStructItems = decls
                |> List.map(mapTypeDecl)
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
