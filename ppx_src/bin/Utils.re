open Ppxlib;
open Parsetree;
open Ast_helper;

let annotationName = "decco";
let encoderFuncSuffix = "_encode";
let decoderFuncSuffix = "_decode";
let encoderVarPrefix = "encoder_";
let decoderVarPrefix = "decoder_";

let loc = default_loc^;

let fail = (loc, message) => Location.raise_errorf(~loc, "%s", message);

[@ocaml.warning("-3")] let longidentParse = Longident.parse

let mkloc = (txt, loc) => {
  {Location.txt, loc};
};
let mknoloc = txt => {
  mkloc(txt, Location.none);
};

let lid = (~loc=Location.none, s) =>
  mkloc(Longident.parse (s), loc)

let makeIdentExpr = (s) =>
  Exp.ident(mknoloc(longidentParse(s)));

let tupleOrSingleton = (tuple, l) =>
    List.length(l) > 1 ? tuple(l) : List.hd(l);

let getAttributeByName = (attributes, name) => {
    let filtered = attributes
        |> List.filter(({attr_name: { Location.txt }}) => txt == name);

    switch filtered {
        | [] => Ok(None)
        | [attribute] => Ok(Some(attribute))
        | _ => Error("Too many occurrences of \"" ++ name ++ "\" attribute")
    };
};

/* TODO: make this not suck */
type generatorSettings = { doEncode: bool, doDecode: bool };
let getGeneratorSettingsFromAttributes = (attributes) =>
    switch (getAttributeByName(attributes, annotationName)) {
        | Ok(None) =>
            switch ((
                getAttributeByName(attributes, annotationName ++ ".decode"),
                getAttributeByName(attributes, annotationName ++ ".encode")))
            {
                | (Ok(Some(_)), Ok(Some(_))) => Ok(Some({ doEncode: true, doDecode: true }))
                | (Ok(Some(_)), Ok(None)) => Ok(Some({ doEncode: false, doDecode: true }))
                | (Ok(None), Ok(Some(_))) => Ok(Some({ doEncode: true, doDecode: false }))
                | (Ok(None), Ok(None)) => Ok(None)
                | (Error(_) as e, _) => e
                | (_, Error(_) as e) => e
            }
        | Ok(Some(_)) => Ok(Some({ doEncode: true, doDecode: true }))
        | Error(_) as e => e
    };

let getExpressionFromPayload = ({ attr_name: { loc }, attr_payload: payload }) =>
    switch payload {
        | PStr([{ pstr_desc }]) => switch pstr_desc {
            | Pstr_eval(expr, _) => expr
            | _ => fail(loc, "Expected expression as attribute payload")
        };
        | _ => fail(loc, "Expected expression as attribute payload")
    };

let getParamNames = (params) =>
    params
    |> List.map((({ ptyp_desc, ptyp_loc }, _)) =>
        switch ptyp_desc {
            | Ptyp_var(s) => s
            | _ => fail(ptyp_loc, "Unhandled param type")
                |> (v) => Location.Error(v)
                |> raise
        }
    );

let indexConst = (i) =>
    Pconst_string("[" ++ string_of_int(i) ++ "]", Location.none, None)
    |> Exp.constant;

let rec isIdentifierUsedInCoreType = (typeName, {ptyp_desc, ptyp_loc}) =>
    switch ptyp_desc {
        | Ptyp_arrow(_, _, _) => fail(ptyp_loc, "Can't generate codecs for function type")
        | Ptyp_any => fail(ptyp_loc, "Can't generate codecs for `any` type")
        | Ptyp_package(_)=> fail(ptyp_loc, "Can't generate codecs for module type")
        | Ptyp_variant(_, _, _) => fail(ptyp_loc, "Unexpected Ptyp_variant")
        | Ptyp_var(_) => false
        | Ptyp_tuple(childTypes) =>
            List.exists(isIdentifierUsedInCoreType(typeName), childTypes)
        | Ptyp_constr({txt}, childTypes) =>
            txt == Lident(typeName)
                ? true
                : List.exists(isIdentifierUsedInCoreType(typeName), childTypes)
        | _ => fail(ptyp_loc, "This syntax is not yet handled by decco")
    };

let attrWarning = expr => {
  {
    attr_name: mkloc("ocaml.warning", loc),
    attr_payload: PStr([{pstr_desc: Pstr_eval(expr, []), pstr_loc: loc}]),
    attr_loc: loc
  };
};
