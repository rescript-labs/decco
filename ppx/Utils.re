open Migrate_parsetree;
open Ast_402;
open Parsetree;
open Ast_helper;

type result('a, 'b) = Ok('a) | Error('b);

let annotationName = "decco";
let encoderFuncSuffix = "_encode";
let decoderFuncSuffix = "_decode";
let encoderVarPrefix = "encoder_";
let decoderVarPrefix = "decoder_";

let fail = (loc, message) =>
    Location.error(~loc, message)
    |> (v) => Location.Error(v)
    |> raise;

let makeIdentExpr = (s) =>
    Longident.parse(s)
    |> Location.mknoloc
    |> Exp.ident;

let tupleOrSingleton = (tuple, l) =>
    List.length(l) > 1 ? tuple(l) : List.hd(l);

let getAttributeByName = (attributes, name) => {
    let filtered = attributes
        |> List.filter((({ Location.txt }, _)) => txt == name);

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

let getExpressionFromPayload = (({ loc } : Location.loc('a), payload)) =>
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
    Asttypes.Const_string("[" ++ string_of_int(i) ++ "]", None)
    |> Exp.constant;