open Ppxlib;


class mapper = {
  as self;
  inherit class Ast_traverse.map;
  pub! signature = signature => {
    signature
    |> List.map(Signature.mapSignatureItem(self))
    |> List.concat;
  };
  pub! structure = structure => {
    structure
    |> List.map(Structure.mapStructureItem(self))
    |> List.concat;
  };
};

let structure_mapper = s => (new mapper)#structure(s);
let signature_mapper = s => (new mapper)#signature(s);

Ppxlib.Driver.register_transformation(
  ~preprocess_impl=structure_mapper,
  ~preprocess_intf=signature_mapper,
  "decco",
);
