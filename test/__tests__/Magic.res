open Jest
open TestUtils

@decco type magic = @decco.codec(Decco.Codecs.magic) int

describe("magic", () => {
  let i = 24
  testGoodDecode("", magic_decode, magic_encode(i), i)
})
