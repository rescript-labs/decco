open Ppxlib
class mapper =
  object (self)
    inherit  Ast_traverse.map
    method! signature signature =
      (signature |> (List.map (Signature.mapSignatureItem self))) |>
        List.concat
    method! structure structure =
      (structure |> (List.map (Structure.mapStructureItem self))) |>
        List.concat
  end
let structure_mapper s = (new mapper)#structure s
let signature_mapper s = (new mapper)#signature s
;;Ppxlib.Driver.register_transformation ~preprocess_impl:structure_mapper
    ~preprocess_intf:signature_mapper "decco"