open Ppxlib;
open Parsetree;
open Ast_helper;
open Codecs;
open Utils;

let addParams = (paramNames, expr) =>
    List.fold_right((s, acc) => {
        let pat = Pat.var(mknoloc(s));
        Exp.fun_(Asttypes.Nolabel, None, pat, acc);
    }, paramNames, [%expr (v) => [%e expr](v)]);

let generateCodecDecls = (typeName, paramNames, (encoder, decoder)) => {
    let encoderPat = Pat.var(mknoloc(typeName ++ Utils.encoderFuncSuffix));
    let encoderParamNames = List.map(s => encoderVarPrefix ++ s, paramNames);

    let decoderPat = Pat.var(mknoloc(typeName ++ Utils.decoderFuncSuffix));
    let decoderParamNames = List.map(s => decoderVarPrefix ++ s, paramNames);

    let vbs = [];

    let vbs = switch encoder {
    | None => vbs
    | Some(encoder) =>
      vbs
      @ [
        Vb.mk(
          ~attrs=[attrWarning([%expr "-39"])],
          encoderPat,
          addParams(encoderParamNames, encoder)
        )
      ]
    };

    let vbs = switch decoder {
    | None => vbs
    | Some(decoder) =>
      vbs
      @ [
        Vb.mk(
          ~attrs=[attrWarning([%expr "-4"]), attrWarning([%expr "-39"])],
          decoderPat,
          addParams(decoderParamNames, decoder),
        ),
      ]
    };

    vbs;
};

let mapTypeDecl = (decl) => {
    let { ptype_attributes, ptype_name: { txt: typeName },
          ptype_manifest, ptype_params, ptype_loc, ptype_kind } = decl;

    let isUnboxed = switch (Utils.getAttributeByName(ptype_attributes, "unboxed")) {
        | Ok(Some(_)) => true
        | _ => false
    };

    switch (getGeneratorSettingsFromAttributes(ptype_attributes)) {
        | Ok(None) => []
        | Ok(Some(generatorSettings)) => switch (ptype_manifest, ptype_kind) {
            | (None, Ptype_abstract) => fail(ptype_loc, "Can't generate codecs for unspecified type")
            | (Some({ ptyp_desc: Ptyp_variant(rowFields, _, _) }), Ptype_abstract) => {
                generateCodecDecls(
                typeName, getParamNames(ptype_params),
                Polyvariants.generateCodecs(generatorSettings, rowFields, isUnboxed)
            )}
            | (Some(manifest), _) => generateCodecDecls(
                typeName, getParamNames(ptype_params),
                generateCodecs(generatorSettings, manifest)
            )
            | (None, Ptype_variant(decls)) => generateCodecDecls(
                typeName, getParamNames(ptype_params),
                Variants.generateCodecs(generatorSettings, decls, isUnboxed)
            )
            | (None, Ptype_record(decls)) => generateCodecDecls(
                typeName, getParamNames(ptype_params),
                Records.generateCodecs(generatorSettings, decls, isUnboxed)
            )
            | _ => fail(ptype_loc, "This type is not handled by decco")
        }
        | Error(s) => fail(ptype_loc, s)
    };
};

let mapStructureItem = (mapper, { pstr_desc } as structureItem) =>
    switch pstr_desc {
        | Pstr_type(recFlag, decls) => {
            let valueBindings = decls
                |> List.map(mapTypeDecl)
                |> List.concat;

            [   mapper#structure_item(structureItem)]
            @ (List.length(valueBindings) > 0 ?
                [ Str.value(recFlag, valueBindings) ]
                : []);
        }

        | _ => [ mapper#structure_item(structureItem) ]
    };
