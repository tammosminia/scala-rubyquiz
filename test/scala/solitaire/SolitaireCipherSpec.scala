package solitaire

import SolitaireCipher._

class SolitaireCipherSpec extends org.scalatest.FunSuite {

  test("pad - example") {
    assert(pad("Code in Ruby, live longer!") === "CODEINRUBYLIVELONGER")
  }

  test("pad - padding to a multiple of 5") {
    assert(pad("az") === "AZXXX")
  }

  test("pad - length 5 doesnt need padding") {
    assert(pad("zxZzz") === "ZXZZZ")
  }


  test("stringToIntList - example") {
    assert(stringToIntList("CODEINRUBYLIVELONGER") === List(3, 15, 4, 5, 9, 14, 18, 21, 2, 25, 12, 9, 22, 5, 12, 15, 14, 7, 5, 18))
  }

  test("stringToIntList - example2") {
    assert(stringToIntList("DWJXHYRFDGTMSHPUURXJ") === List(4, 23, 10, 24, 8, 25, 18, 6, 4, 7, 20, 13, 19, 8, 16, 21, 21, 18, 24, 10))
  }

  test("stringToIntList - empty") {
    assert(stringToIntList("") === List())
  }

  test("stringToIntList - A") {
    assert(stringToIntList("A") === List(1))
  }

  test("stringToIntList - ABCDE") {
    assert(stringToIntList("ABCDE") === List(1, 2, 3, 4, 5))
  }


  test("intListToString - example") {
    assert(intListToString(List(7, 12, 14, 3, 17, 13, 10, 1, 6, 6, 6, 22, 15, 13, 2, 10, 9, 25, 3, 2)) === "GLNCQMJAFFFVOMBJIYCB")
  }

  test("intListToString - example2") {
    assert(intListToString(List(3, 15, 4, 5, 9, 14, 18, 21, 2, 25, 12, 9, 22, 5, 12, 15, 14, 7, 5, 18)) === "CODEINRUBYLIVELONGER")
  }

  test("intListToString - empty") {
    assert(intListToString(Nil) === "")
  }

  test("intListToString - a") {
    assert(intListToString(List(1)) === "A")
  }


  test("encodeInner - example") {
    val message = List(3, 15, 4, 5, 9, 14, 18, 21, 2, 25, 12, 9, 22, 5, 12, 15, 14, 7, 5, 18)
    val cipher = List(4, 23, 10, 24, 8, 25, 18, 6, 4, 7, 20, 13, 19, 8, 16, 21, 21, 18, 24, 10)
    val encoded = List(7, 12, 14, 3, 17, 13, 10, 1, 6, 6, 6, 22, 15, 13, 2, 10, 9, 25, 3, 2)
    assert(encodeInner(message, cipher) === encoded)
  }

  test("encodeInner - incompatible lengths") {
    intercept[RuntimeException] {
      encodeInner(List(), List(1))
    }
    intercept[RuntimeException] {
      encodeInner(List(1), List())
    }
  }

  test("encodeInner - empty") {
    assert(encodeInner(Nil, Nil) === Nil)
  }

  test("encodeInner - a + a") {
    assert(encodeInner(List(1), List(1)) === List(2))
  }

  test("encodeInner - a + z") {
    assert(encodeInner(List(1), List(26)) === List(1))
  }


  test("decodeInner - example") {
    val message = List(3, 15, 4, 5, 9, 14, 18, 21, 2, 25, 12, 9, 22, 5, 12, 15, 14, 7, 5, 18)
    val cipher = List(4, 23, 10, 24, 8, 25, 18, 6, 4, 7, 20, 13, 19, 8, 16, 21, 21, 18, 24, 10)
    val encoded = List(7, 12, 14, 3, 17, 13, 10, 1, 6, 6, 6, 22, 15, 13, 2, 10, 9, 25, 3, 2)
    assert(decodeInner(encoded, cipher) === message)
  }

  test("decodeInner - incompatible lengths") {
    intercept[RuntimeException] {
      decodeInner(List(), List(1))
    }
    intercept[RuntimeException] {
      decodeInner(List(1), List())
    }
  }

  test("decodeInner - empty") {
    assert(decodeInner(Nil, Nil) === Nil)
  }

  test("decodeInner - a + a") {
    assert(decodeInner(List(2), List(1)) === List(1))
  }

  test("decodeInner - a + z") {
    assert(decodeInner(List(1), List(26)) === List(1))
  }

  //For the sake of testing, the first ten output letters for an unkeyed deck are:
  //  D (4)  W (49)  J (10)  Skip Joker (53)  X (24)  H (8)
  //  Y (51)  R (44)  F (6)  D (4)  G (33)
  test("generateKey - example") {
    assert(generateKey(Deck.initialDeck, 10) === "DWJXHYRFDG")
  }

  //CLEPKHHNIYCFPWHFDFEH -> YOURCIPHERISWORKINGX
  //ABVAWLWZSYOORYKDUPVH -> WELCOMETORUBYQUIZXXX
  test("decode - example") {
    assert(decode("CLEPKHHNIYCFPWHFDFEH", Deck.initialDeck) === "YOURCIPHERISWORKINGX")
    assert(decode("ABVAWLWZSYOORYKDUPVH", Deck.initialDeck) === "WELCOMETORUBYQUIZXXX")
  }



}
