open Jest;
open Expect;
open TestUtils;
/* open Decco; /* Don't open these in order to validate ppx works without it */
   open Belt.Result; */

[@decco]
type s = string;
[@decco]
type i = int;
[@decco]
type i64 = int64;
[@decco]
type i64Unsafe = [@decco.codec Decco.Codecs.int64Unsafe] int64;
[@decco]
type f = float;
[@decco]
type b = bool;
[@decco]
type u = unit;
[@decco]
type t = (int, string);
[@decco]
type a('a) = array('a);
[@decco]
type l('a) = list('a);
[@decco]
type o('a) = option('a);
[@decco]
type r('v, 'e) = Belt.Result.t('v, 'e);
[@decco]
type d('v) = Js.Dict.t('v);
[@decco]
type simpleVar('a) = 'a;
[@decco]
type j = Js.Json.t;
[@decco]
type optionList = l(o(s));
[@decco]
type dictInt = d(int);

[@decco]
type record = {
  hey: s,
  opt: option(int),
  o: o(i),
  [@decco.default 1.0]
  f: float,
  [@decco.key "other_key"]
  otherKey: string,
};

[@decco]
type bigV =
  | V(array(option(list(string))));
[@decco]
type bigR = {bigV};

[@decco]
type falseable('a) = [@decco.codec Decco.Codecs.falseable] option('a);
[@decco]
type magic = [@decco.codec Decco.Codecs.magic] int;

module type TestMod = {
  [@decco]
  type t;
  [@decco]
  type varType('a, 'b);

  let mkT: string => t;
  let mkVarType: ('a, 'b) => varType('a, 'b);
};

module TestMod: TestMod = {
  [@decco]
  type t = string;
  [@decco]
  type varType('a, 'b) = ('a, 'b);

  let mkT = (s: string): t => s;
  let mkVarType = (a, b) => (a, b);
};

[@decco]
type dependentOnTestMod = TestMod.t;

module type EncOnly = {
  [@decco.encode]
  type t;
};
module EncOnly: EncOnly = {
  let t_decode = 1;
  [@decco.encode]
  type t = int;
  t_decode + 1;
  /** this won't typecheck if t_decode is generated  */;
};

module type DecOnly = {
  [@decco.decode]
  type t;
};
module DecOnly: DecOnly = {
  let t_encode = 1;
  [@decco.decode]
  type t = int;
  t_encode + 1;
};

[@decco]
type polyvariant = [ | `A | `B(i) | `C(i, s)];

describe("polyvariant", () => {
  describe("polyvariant_encode", ()
    => {
      testEncode("A", `A, polyvariant_encode, {|["A"]|});
      testEncode("B", `B(5), polyvariant_encode, {|["B",5]|});
      testEncode("C", `C((7, "8")), polyvariant_encode, {|["C",7,"8"]|});
    })
    describe("polyvariant_decode", () => {
       describe("good", () => {
         let json = {|["A"]|} |> Js.Json.parseExn;
         testGoodDecode("A", polyvariant_decode, json, `A);
         let json = {|["B",5]|} |> Js.Json.parseExn;
         testGoodDecode("B", polyvariant_decode, json, `B(5));
         let json = {|["C",7,"8"]|} |> Js.Json.parseExn;
         testGoodDecode("C", polyvariant_decode, json, `C((7, "8")));
       });
    /*     describe("bad", () => {
               testBadDecode(
                 "non-polyvariant",
                 polyvariant_decode,
                 Js.Json.number(12.),
                 {path: "", message: "Not a polyvariant", value: Js.Json.number(12.)},
               );

               let json = {|["D"]|} |> Js.Json.parseExn;
               testBadDecode(
                 "bad constructor",
                 polyvariant_decode,
                 json,
                 {
                   path: "",
                   message: "Invalid polyvariant constructor",
                   value: Js.Json.string("D"),
                 },
               );

               let json = {|["A",1]|} |> Js.Json.parseExn;
               testBadDecode(
                 "too many arguments",
                 polyvariant_decode,
                 json,
                 {
                   path: "",
                   message: "Invalid number of arguments to polyvariant constructor",
                   value: json,
                 },
               );

               let json = {|[]|} |> Js.Json.parseExn;
               testBadDecode(
                 "no arguments",
                 polyvariant_decode,
                 json,
                 {
                   path: "",
                   message: "Expected polyvariant, found empty array",
                   value: json,
                 },
               );

               let json = {|["B"]|} |> Js.Json.parseExn;
               testBadDecode(
                 "not enough arguments",
                 polyvariant_decode,
                 json,
                 {
                   path: "",
                   message: "Invalid number of arguments to polyvariant constructor",
                   value: json,
                 },
               );

               let json = {|["B","oh"]|} |> Js.Json.parseExn;
               testBadDecode(
                 "invalid argument",
                 polyvariant_decode,
                 json,
                 {path: "[0]", message: "Not a number", value: Js.Json.string("oh")},
               );
             });
           }); */
});
