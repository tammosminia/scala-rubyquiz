package solitaire

object Suit extends Enumeration {
  type Suit = Value
  val Clubs, Diamonds, Hearts, Spades = Value

  def valueOf(suit: Suit) = suit match {
    case Clubs => 0
    case Diamonds => 13
    case Hearts => 26
    case Spades => 39
  }
}
import solitaire.Suit._

abstract class Card
//ace=1, jack=11, queen=12, king=13
case class SuitedCard(suit: Suit, number: Int) extends Card
case object JokerA extends Card
case object JokerB extends Card

object Deck {
  val initialDeck: List[Card] ={
    val initialSuits = Suit.values.toList.flatMap { suit =>
      (1 to 13).map { number => SuitedCard(suit, number) }
    }
    initialSuits :+ JokerA :+ JokerB
  }

  def removeCard(card: Card, input: List[Card]) = {
    input diff List(card)
  }

  def insertCard(card: Card, index: Int, input: List[Card]) = {
    (input.slice(0, index) :+ card) ++ input.drop(index)
  }

  def insertCardAfter(card: Card, index: Int, input: List[Card]) = {
    (input.slice(0, index + 1) :+ card) ++ input.drop(index + 1)
  }

  def moveJokerA(input: List[Card]): List[Card] = {
    moveCardDown(JokerA, input)
  }

  def moveJokerB(input: List[Card]): List[Card] = {
    moveCardDown(JokerB, input, 2)
  }

  def moveCardDown(card: Card, input: List[Card], places: Int = 1): List[Card] = {
    val index = input.indexOf(card)
    val removed = removeCard(card, input)
    val newIndex = (index + places - 1) % removed.size //at this point there are only 53 cards(not counting the joker)
    insertCardAfter(card, newIndex, removed)
  }

  def tripleCut(input: List[Card]): List[Card] = {
    val indexes = List(JokerA, JokerB).map { input.indexOf(_)}
    val lowestIndex = indexes.min
    val highestIndex = indexes.max
    val before = input.slice(0, lowestIndex)
    val inbetween = input.slice(lowestIndex, highestIndex + 1)
    val after = input.drop(highestIndex + 1)
    after ++ inbetween ++ before
  }

  private def cardValue(card: Card): Int = card match {
    case _: SuitedCard => initialDeck.indexOf(card) + 1
    case _ => 53
  }

  private def cardToLetter(card: Card): Option[Char] = card match {
    case _: SuitedCard => Some(('A' + (cardValue(card) % 26) - 1).toChar)
    case _ => None
  }

  def countCut(input: List[Card]): List[Card] = {
    val index = cardValue(input.last)
    val before = input.slice(0, index)
    val inbetween = input.slice(index, input.size - 1)
    val after = List(input.last)
    inbetween ++ before ++ after
  }

  def generateLetter(input: List[Card]): Option[Char] = {
    val index = cardValue(input.head)
    val card = input(index)
    cardToLetter(card)
  }


}

class Deck(var cards: List[Card]) {
  def once(): Option[Char] = {
    cards = Deck.moveJokerA(cards)
    cards = Deck.moveJokerB(cards)
    cards = Deck.tripleCut(cards)
    cards = Deck.countCut(cards)
    Deck.generateLetter(cards)
  }

  def oneLetter(): Char = once() match {
    case Some(letter) => letter
    case None => oneLetter()
  }
}


