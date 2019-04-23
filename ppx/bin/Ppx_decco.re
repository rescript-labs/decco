open Migrate_parsetree;
open Ast_402;
open Ast_mapper;

let mapStructure = (mapper, structure) =>
    structure
    |> List.map(Structure.mapStructureItem(mapper))
    |> List.concat;

let mapSignature = (mapper, signature) =>
    signature
    |> List.map(Signature.mapSignatureItem(mapper))
    |> List.concat;

let mapper = (_, _) => {
    ...default_mapper,
    structure: mapStructure,
    signature: mapSignature
};

Driver.register(~name="decco", Versions.ocaml_402, mapper);
