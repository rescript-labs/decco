open Migrate_parsetree;
open Ast_402;
open Ppx_tools_402;
open Ast_mapper;
open Parsetree;

let annotationName = "decco";

let generateCodecs = (typeName, manifest) =>
    switch manifest.ptyp_desc {
        | Ptyp_any => failwith("Can't generate codecs for `any` type")
        | Ptyp_var(_) => failwith("Can't generate codecs for variable type")
        | Ptyp_alias(_, _) => failwith("Can't generate codecs for aliased type")
        | Ptyp_constr({ txt: identifier }, typeArgs) => {
            let toJsonPat = Ast_helper.Pat.var(Location.mknoloc(typeName ++ "__to_json"));
            switch (Longident.flatten(identifier)) {
                | [ "string" ] => [%str let [%p toJsonPat] = (v) => Decco.string_to_json(v); ]
                | [ "int" ] => [%str let [%p toJsonPat] = (v) => Decco.int_to_json(v); ]
                | [ "float" ] => [%str let [%p toJsonPat] = (v) => Decco.float_to_json(v); ]
                | [ "bool" ] => [%str let [%p toJsonPat] = (v) => Decco.bool_to_json(v); ]
                | [ "unit" ] => [%str let [%p toJsonPat] = (v) => Decco.unit_to_json(v); ]
                | _ => raise(Location.Error(Location.error(~loc=manifest.ptyp_loc, "not implemented")))
            }
        }
        | _ => failwith("noo")
    };

let mapTypeDecl = (mapper, decl) => {
    let { ptype_attributes, ptype_name: { txt: typeName }, ptype_manifest } = decl;

    let matchingAttributes = ptype_attributes
        |> List.filter((({ Location.txt }, _)) => txt == annotationName);

    switch matchingAttributes {
        | [] => []
        | _ =>
            switch ptype_manifest {
                | None => failwith("Can't generate codecs for unspecified type")
                | Some(manifest) => generateCodecs(typeName, manifest)
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
