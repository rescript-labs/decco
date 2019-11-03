open Migrate_parsetree;
open Ast_406;
open Ast_mapper;
open Parsetree;
open Ast_helper;
open Codecs;
open Utils;

let addParams = (paramNames, expr) =>
    List.fold_right((s, acc) => {
        let pat = Pat.var(Location.mknoloc(s));
        Exp.fun_(Asttypes.Nolabel, None, pat, acc);
    }, paramNames, [%expr (v) => [%e expr](v)]);

let generateCodecDecls = (typeName, paramNames, (encoder, decoder)) => {
    let encoderPat = Pat.var(Location.mknoloc(typeName ++ Utils.encoderFuncSuffix));
    let encoderParamNames = List.map(s => encoderVarPrefix ++ s, paramNames);

    let decoderPat = Pat.var(Location.mknoloc(typeName ++ Utils.decoderFuncSuffix));
    let decoderParamNames = List.map(s => decoderVarPrefix ++ s, paramNames);

    let vbs = [];

    let vbs = switch encoder {
    | None => vbs
    | Some(encoder) => vbs @ [Vb.mk(encoderPat, addParams(encoderParamNames, encoder))]
    };

    let vbs = switch decoder {
    | None => vbs
    | Some(decoder) => vbs @ [Vb.mk(decoderPat, addParams(decoderParamNames, decoder))]
    };

    [Str.value(Asttypes.Nonrecursive, vbs)];
};

let mapTypeDecl = (decl) => {
    let { ptype_attributes, ptype_name: { txt: typeName },
          ptype_manifest, ptype_params, ptype_loc, ptype_kind } = decl;

    switch (getGeneratorSettingsFromAttributes(ptype_attributes)) {
        | Ok(None) => []
        | Ok(Some(generatorSettings)) => switch (ptype_manifest, ptype_kind) {
            | (None, Ptype_abstract) => fail(ptype_loc, "Can't generate codecs for unspecified type")

            | (Some(manifest), _) => generateCodecDecls(
                typeName, getParamNames(ptype_params),
                generateCodecs(generatorSettings, manifest)
            )
            | (None, Ptype_variant(decls)) => generateCodecDecls(
                typeName, getParamNames(ptype_params),
                Variants.generateCodecs(generatorSettings, decls)
            )
            | (None, Ptype_record(decls)) => generateCodecDecls(
                typeName, getParamNames(ptype_params),
                Records.generateCodecs(generatorSettings, decls)
            )
            | _ => fail(ptype_loc, "This type is not handled by decco")
        }
        | Error(s) => fail(ptype_loc, s)
    };
};

let mapStructureItem = (mapper, { pstr_desc } as structureItem) =>
    switch pstr_desc {
        | Pstr_type(_, decls) => {
            let generatedStructItems = decls
                |> List.map(mapTypeDecl)
                |> List.concat;

            [   mapper.structure_item(mapper, structureItem),
                ...generatedStructItems ];
        }

        | _ => [ mapper.structure_item(mapper, structureItem) ]
    };
