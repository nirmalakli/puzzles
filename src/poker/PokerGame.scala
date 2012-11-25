package poker

object PokerGame extends App {
  val poker = new PokerGame
  val hands : List[(Hand, Hand)] = poker.listOfHands
  val player1WinsList = hands.filter(x => x._1.compareTo(x._2) == Greater)
  val player1WinsCount = if (player1WinsList isEmpty) 0 else player1WinsList.size
  println("Player-1 wins " + player1WinsCount + " times.")
}

class PokerGame {

  def stringToCard(data: String): Card = {

    val faceValueChar: Char = data(0)
    val faceValue = if (faceValueChar == 'T') "10" else faceValueChar.toString
    val suitChar: Char = data(1)
    val suit: Suit = suitChar match {
      case 'D' => Diamond
      case 'S' => Spade
      case 'H' => Heart
      case 'C' => Club
      case x => throw new IllegalArgumentException("Invalid suit!")
    }

    new Card(suit, faceValue)
  }

  def readLine(data: String): (Hand, Hand) = {
    val tokens: Array[String] = data.split(" ")
    val hand1 = for { i <- 0 to 4 } yield tokens(i)
    val hand2 = for { i <- 5 to 9 } yield tokens(i)

    val cards1 = hand1.toList.map(stringToCard)
    val cards2 = hand2.toList.map(stringToCard)
    (Hand(cards1), Hand(cards2))
  }

  def listOfHands() = {
    val lines = scala.io.Source.fromFile("src/poker/poker.txt").getLines
    val hands =
      for {
        line <- lines
        val (hand1, hand2) = readLine(line)
      } yield (hand1, hand2)

    hands.toList

  }
}