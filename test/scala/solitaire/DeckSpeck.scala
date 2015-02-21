package solitaire

import org.scalatest.FunSuite
import Deck._

class DeckSpeck extends FunSuite {
  test("initialDeck - has 54 cards") {
    assert(initialDeck.size === 54)
  }

  test("initialDeck - starts with ace of clubs") {
    assert(initialDeck.head === SuitedCard(Suit.Clubs, 1))
  }

  test("initialDeck - ends with JokerB") {
    assert(initialDeck.last === JokerB)
  }


  test("removeCard - first") {
    val result = removeCard(initialDeck.head, initialDeck)
    assert(result.size === 53)
    assert(result(0) === initialDeck(1))
  }

  test("removeCard - second") {
    val result = removeCard(initialDeck(1), initialDeck)
    assert(result.size === 53)
    assert(result(0) === initialDeck(0))
    assert(result(1) === initialDeck(2))
  }

  test("removeCard - last") {
    val result = removeCard(initialDeck.last, initialDeck)
    assert(result.size === 53)
    assert(result.last === JokerA)
  }


  test("insertCard - first") {
    val result = insertCard(JokerA, 0, initialDeck)
    assert(result.size === 55)
    assert(result(0) === JokerA)
    assert(result(1) === initialDeck(0))
  }

  test("insertCard - second") {
    val result = insertCard(JokerA, 1, initialDeck)
    assert(result.size === 55)
    assert(result(0) === initialDeck(0))
    assert(result(1) === JokerA)
    assert(result(2) === initialDeck(1))
  }

  test("insertCard - last - does this make sense?") {
    val result = insertCard(JokerA, 54, initialDeck)
    assert(result.size === 55)
    assert(result(54) === JokerA)
    assert(result(53) === JokerB)
  }


  test("insertCardAfter - first") {
    val result = insertCardAfter(JokerA, 0, initialDeck)
    assert(result.size === 55)
    assert(result(0) === initialDeck(0))
    assert(result(1) === JokerA)
    assert(result(2) === initialDeck(1))
  }

  test("insertCardAfter - last") {
    val result = insertCardAfter(JokerA, 53, initialDeck)
    assert(result.size === 55)
    assert(result(54) === JokerA)
    assert(result(53) === JokerB)
  }


  test("moveJokerA - on initialDeck") {
    val result = moveJokerA(initialDeck)
    assert(result.size === 54)
    assert(result.last === JokerA)
  }

  test("moveJokerA - twice - should be second card") {
    val result = moveJokerA(moveJokerA(initialDeck))
    assert(result.size === 54)
    assert(result(0) === initialDeck(0))
    assert(result(1) === JokerA)
  }


  test("moveJokerB - on initialDeck") {
    val result = moveJokerB(initialDeck)
    assert(result.size === 54)
    assert(result.last === JokerA)
    assert(result(0) === initialDeck(0))
    assert(result(1) === initialDeck(1))
    assert(result(2) === JokerB)
  }

  test("moveJokerB - after moveJokerA") {
    val result = moveJokerB(moveJokerA(initialDeck))
    assert(result.size === 54)
    assert(result.last === JokerA)
    assert(result(0) === initialDeck(0))
    assert(result(1) === JokerB)
    assert(result(2) === initialDeck(1))
  }


  test("tripleCut - example") {
    val result = tripleCut(moveJokerB(moveJokerA(initialDeck)))
    assert(result.size === 54)
    assert(result(0) === JokerB)
    assert(result(1) === initialDeck(1))
    assert(result(2) === initialDeck(2))
    assert(result(51) === initialDeck(51))
    assert(result(52) === JokerA)
    assert(result(53) === initialDeck(0))
  }

  test("tripleCut - 1 2 A ... B 4 5") {
    val input = List(initialDeck(0), initialDeck(1), JokerA, initialDeck(2), initialDeck(3), JokerB, initialDeck(4), initialDeck(5))
    val result = tripleCut(input)
    assert(result === List(initialDeck(4), initialDeck(5), JokerA, initialDeck(2), initialDeck(3), JokerB, initialDeck(0), initialDeck(1)))
  }

  test("tripleCut - A ... B") {
    val input = List(JokerA, initialDeck(2), initialDeck(3), JokerB)
    val result = tripleCut(input)
    assert(result === input)
  }

  test("tripleCut - A B 1") {
    val input = List(JokerA, JokerB, initialDeck(0))
    val result = tripleCut(input)
    assert(result === List(initialDeck(0), JokerA, JokerB))
  }

  //5. Perform a count cut using the value of the bottom card. Cut the bottom card's value in cards off the top of the deck and reinsert them just above the bottom card.
  // This changes our deck to:
  //  2 3 4 ... 52 A B 1  (the 1 tells us to move just the B)
  test("countCut - example") {
    val result = countCut(tripleCut(moveJokerB(moveJokerA(initialDeck))))
    assert(result.size === 54)
    assert(result(0) === initialDeck(1))
    assert(result(1) === initialDeck(2))
    assert(result(50) === initialDeck(51))
    assert(result(51) === JokerA)
    assert(result(52) === JokerB)
    assert(result(53) === initialDeck(0))
  }

  test("countCut - 2") {
    val input = List(JokerA, JokerB, initialDeck(0), initialDeck(1))
    val result = countCut(input)
    assert(result === List(initialDeck(0), JokerA, JokerB, initialDeck(1)))
  }

  test("countCut - initialDeck") {
    val result = countCut(initialDeck)
    assert(result === initialDeck)
  }

  test("countCut - after moveJokerA") {
    val input = moveJokerA(initialDeck)
    val result = countCut(input)
    assert(result === input)
  }

  //6. Find the output letter. Convert the top card to it's value and count down that many cards from the top of the deck,
  // with the top card itself being card number one. Look at the card immediately after your count and convert it to a letter.
  // This is the next letter in the keystream. If the output card is a joker, no letter is generated this sequence.
  // This step does not alter the deck.
  // For our example, the output letter is:
  //  D  (the 2 tells us to count down to the 4, which is a D)
  test("generateLetter - example") {
    val result = generateLetter(countCut(tripleCut(moveJokerB(moveJokerA(initialDeck)))))
    assert(result.get === 'D')
  }

  test("generateLetter - initialDeck") {
    val result = generateLetter(initialDeck)
    assert(result.get === 'B')
  }

  test("generateLetter - ace of spades") {
    val input = List(initialDeck(0), SuitedCard(Suit.Spades, 1))
    val result = generateLetter(input)
    assert(result.get === 'N')
  }

  test("generateLetter - joker") {
    val input = List(initialDeck(0), JokerA)
    val result = generateLetter(input)
    assert(result === None)
  }

  //For the sake of testing, the first ten output letters for an unkeyed deck are:
  //  D (4)  W (49)  J (10)  Skip Joker (53)  X (24)  H (8)
  //  Y (51)  R (44)  F (6)  D (4)  G (33)
  test("once - example") {
    val deck = new Deck(initialDeck)
    assert(deck.once().get === 'D')
    assert(deck.once().get === 'W')
    assert(deck.once().get === 'J')
    assert(deck.once() === None)
    assert(deck.once().get === 'X')
    assert(deck.once().get === 'H')
    assert(deck.once().get === 'Y')
    assert(deck.once().get === 'R')
    assert(deck.once().get === 'F')
    assert(deck.once().get === 'D')
    assert(deck.once().get === 'G')
  }


}
