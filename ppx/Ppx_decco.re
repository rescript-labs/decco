open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Ast_mapper;
open Parsetree;
open Ast_helper;
open Variants;
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

let updateTypeDeclStructure = (typeName, paramNames, decl, loc) => {
    let (encoder, decoder) = switch decl {
        | CoreType(t) => generateCodecs(t, loc);
        | Kind(Ptype_variant(decls)) => generateVariantCodecs(decls)
        | _ => Utils.fail(loc, "Unhandled type syntax")
    };

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
            [Longident.Lapply(Longident.Lident("one"), Longident.Lident("two"))
                |> Location.mknoloc
                |> Exp.ident
                |> Ast_helper.Str.eval];
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
