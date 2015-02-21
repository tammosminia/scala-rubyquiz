package solitaire

object SolitaireCipher {

  def pad(input: String): String = {
    val filtered = input.toUpperCase.filter { letter => ('A' to 'Z').contains(letter) }
    val padding: Int = (5 - filtered.length % 5) % 5
    filtered + "X" * padding
  }

  def stringToIntList(input: String): List[Int] = {
    input.map { letter => (letter - 'A' + 1).toInt }.toList
  }
  def intListToString(input: List[Int]): String = {
    val charList = input.map { int => ('A' - 1 + int).toChar }
    new String(charList.toArray)
  }

  def encodeInner(message: List[Int], cipher: List[Int]): List[Int] = {
    if (message.length != cipher.length) {
      throw new RuntimeException("Incompatible lengths")
    }
    message.zip(cipher).map { case (messageInt, cipherInt) =>
      messageInt + cipherInt match {
        case encodedInt if encodedInt > 26 => encodedInt - 26
        case encodedInt => encodedInt
      }
    }
  }

  def decodeInner(encoded: List[Int], cipher: List[Int]): List[Int] = {
    if (encoded.length != cipher.length) {
      throw new RuntimeException("Incompatible lengths")
    }
    encoded.zip(cipher).map { case (encodedInt, cipherInt) =>
      encodedInt - cipherInt match {
        case messageInt if messageInt < 1 => messageInt + 26
        case messageInt => messageInt
      }
    }
  }

  def generateKey(cards: List[Card], keySize: Int): String = {
    val deck = new Deck(cards)
    val charList = (1 to keySize).map { _ =>
      deck.oneLetter()
    }
    new String(charList.toArray)
  }

  def decode(encoded: String, cards: List[Card]): String = {
    val key = generateKey(cards, encoded.size)
    intListToString(decodeInner(stringToIntList(encoded), stringToIntList(key)))
  }



}
