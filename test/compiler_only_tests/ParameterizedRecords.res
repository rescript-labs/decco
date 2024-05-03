// At one point we had a regression where
// the PPX failed to generate the type parameters
// for the type of the decoder when using a parameterized
// record. This test ensures that the issue is fixed.
@decco
type t<'param> = {blob: 'param}
