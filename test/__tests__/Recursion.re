open Jest;
open TestUtils;

module type Rec = {
    [@decco] type basic = Basic(basic) | End;
    [@decco] type nested = Nested(option(nested));
    [@decco] type tuple = Tuple((int, tuple)) | End;
    [@decco] type record = { r: option(record) };
};

module Rec: Rec = {
    [@decco] type basic = Basic(basic) | End;
    [@decco] type nested = Nested(option(nested));
    [@decco] type tuple = Tuple((int, tuple)) | End;
    [@decco] type record = { r: option(record) };
};

module MutuallyRec = {
    [@decco] type inttree =
        | Empty
        | Node(node)
    [@decco] and node = {
        value: int,
        left: inttree,
        right: inttree,
    };
};

[@decco] type nonRecVariant = int;
[@decco] type nonRecRecord = int;

module type NonRec = {
    [@decco] type nonrec nonRecVariant = Var(nonRecVariant);
    [@decco] type nonrec nonRecRecord = { num: nonRecRecord };
};

module NonRec: NonRec = {
    [@decco] type nonrec nonRecVariant = Var(nonRecVariant);
    [@decco] type nonrec nonRecRecord = { num: nonRecRecord };
};

describe("variant", () => {
    describe("basic", () => {
        let v = Rec.Basic(Basic(End));
        let jsonStr = {|["Basic",["Basic",["End"]]]|};

        testEncode("encode", v, Rec.basic_encode, jsonStr);
        testGoodDecode("decode", Rec.basic_decode, Js.Json.parseExn(jsonStr), v);
    });

    describe("nested", () => {
        let v = Rec.Nested(Some(Nested(None)));
        let jsonStr = {|["Nested",["Nested",null]]|};

        testEncode("encode", v, Rec.nested_encode, jsonStr);
        testGoodDecode("decode", Rec.nested_decode, Js.Json.parseExn(jsonStr), v);
    });

    describe("tuple", () => {
        let v = Rec.Tuple((0, Tuple((1, End))));
        let jsonStr = {|["Tuple",[0,["Tuple",[1,["End"]]]]]|};

        testEncode("encode", v, Rec.tuple_encode, jsonStr);
        testGoodDecode("decode", Rec.tuple_decode, Js.Json.parseExn(jsonStr), v);
    });

    describe("nonrec", () => {
        let v = NonRec.Var(5);
        let jsonStr = {|["Var",5]|};

        testEncode("encode", v, NonRec.nonRecVariant_encode, jsonStr);
        testGoodDecode("decode", NonRec.nonRecVariant_decode, Js.Json.parseExn(jsonStr), v);
    });
});

describe("record", () => {
    describe("rec", () => {
        let v = { Rec.r: Some({ r: None }) };
        let jsonStr = {|{"r":{"r":null}}|};

        testEncode("encode", v, Rec.record_encode, jsonStr);
        testGoodDecode("decode", Rec.record_decode, Js.Json.parseExn(jsonStr), v);
    });

    describe("nonrec", () => {
        let v = { NonRec.num: 72 };
        let jsonStr = {|{"num":72}|};

        testEncode("encode", v, NonRec.nonRecRecord_encode, jsonStr);
        testGoodDecode("decode", NonRec.nonRecRecord_decode, Js.Json.parseExn(jsonStr), v);
    });
});

describe("mutually recursive", () => {
    describe("basic", () => {
        let v = MutuallyRec.Node({
            value: 0,
            left: Empty,
            right: Empty,
        });
        let jsonStr = {|["Node",{"value":0,"left":["Empty"],"right":["Empty"]}]|};

        testEncode("encode", v, MutuallyRec.inttree_encode, jsonStr);
        testGoodDecode("decode", MutuallyRec.inttree_decode, Js.Json.parseExn(jsonStr), v);
    });

    describe("nested", () => {
        let v = MutuallyRec.Node({
            value: 0,
            left: Node({
                value: 1,
                left: Empty,
                right: Empty,
            }),
            right: Node({
                value: 2,
                left: Empty,
                right: Empty,
            }),
        });
        let jsonStr = {|["Node",{"value":0,"left":["Node",{"value":1,"left":["Empty"],"right":["Empty"]}],"right":["Node",{"value":2,"left":["Empty"],"right":["Empty"]}]}]|};

        testEncode("encode", v, MutuallyRec.inttree_encode, jsonStr);
        testGoodDecode("decode", MutuallyRec.inttree_decode, Js.Json.parseExn(jsonStr), v);
    });
});
