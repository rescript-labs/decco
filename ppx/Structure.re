open Migrate_parsetree;
open Ast_402;
open Ast_mapper;
open Parsetree;
open Ast_helper;
open Codecs;
open Utils;

let addParams = (paramNames, expr) =>
    List.fold_right((s, acc) => {
        let pat = Pat.var(Location.mknoloc(s));
        Exp.fun_("", None, pat, acc);
    }, paramNames, [%expr (v) => [%e expr](v)]);

let generateCodecDecls = (typeName, paramNames, (encoder, decoder)) => {
    let encoderPat = Pat.var(Location.mknoloc(typeName ++ Utils.encoderFuncSuffix));
    let encoderParamNames = List.map(s => encoderVarPrefix ++ s, paramNames);

    let decoderPat = Pat.var(Location.mknoloc(typeName ++ Utils.decoderFuncSuffix));
    let decoderParamNames = List.map(s => decoderVarPrefix ++ s, paramNames);

    [%str let ([%p encoderPat], [%p decoderPat]) = (
        [%e addParams(encoderParamNames, encoder)],
        [%e addParams(decoderParamNames, decoder)]
    )];
};

let mapTypeDecl = (decl) => {
    let { ptype_attributes, ptype_name: { txt: typeName },
          ptype_manifest, ptype_params, ptype_loc, ptype_kind } = decl;

    let matchingAttributes = getAttributeByName(ptype_attributes, annotationName);

    switch matchingAttributes {
        | Ok(None) => []
        | Ok(_) => switch (ptype_manifest, ptype_kind) {
            | (None, Ptype_abstract) => fail(ptype_loc, "Can't generate codecs for unspecified type")

            | (Some(manifest), _) => generateCodecDecls(
                typeName, getParamNames(ptype_params), generateCodecs(manifest)
            )
            | (None, Ptype_variant(decls)) => generateCodecDecls(
                typeName, getParamNames(ptype_params),
                Variants.generateCodecs(decls)
            )
            | (None, Ptype_record(decls)) => generateCodecDecls(
                typeName, getParamNames(ptype_params),
                Records.generateCodecs(decls)
            )
            | _ => fail(ptype_loc, "This type is not handled by decco")
        }
        | Error(s) => fail(ptype_loc, s)
    };
};

let mapStructureItem = (mapper, { pstr_desc } as structureItem) =>
    switch pstr_desc {
        | Pstr_type(decls) => {
            let generatedStructItems = decls
                |> List.map(mapTypeDecl)
                |> List.concat;

            [   mapper.structure_item(mapper, structureItem),
                ...generatedStructItems ];
        }

        | _ => [ mapper.structure_item(mapper, structureItem) ]
    };