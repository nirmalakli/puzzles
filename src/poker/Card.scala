package poker

class Card(val suit: Suit, val faceValueOrig: String) {
  val faceValue : String = faceValueOrig.toUpperCase
  
  val integerFaceValue: Int = faceValue match {
    case "J" => 11
    case "Q" => 12
    case "K" => 13
    case "1" => 14
    case "A" => 14
    case x if x.toInt >= 2 && x.toInt <=10 => x.toInt
    case x => throw new IllegalArgumentException("Invalid face value(" + faceValueOrig + "). Valid values are (2,3,..10, J, Q, K, A)")
  }

  def < (other:Card) : Boolean = integerFaceValue < other.integerFaceValue 
  def > (other:Card) : Boolean = integerFaceValue > other.integerFaceValue 
  def == (other:Card) : Boolean = suit == other.suit && integerFaceValue == other.integerFaceValue
  def compareTo(other: Card) : ComparisionResult = {
    if( this == other) Equals
    else if(this < other) Smaller
    else Greater
  }
  override def toString() : String = "Card[" + suit + ", " + faceValue + "]"
}

trait Suit
case object Heart extends Suit
case object Spade extends Suit
case object Club extends Suit
case object Diamond extends Suit

trait ComparisionResult
case object Smaller extends ComparisionResult
case object Greater extends ComparisionResult
case object Equals extends ComparisionResult

case class Hand(val cards : List[Card]) {
  
  val sortBySuit = {
    val hearts = cards filter (x => x.suit == Heart) 
    val spades = cards filter (y => y.suit == Spade) 
    val diamonds = cards filter (y => y.suit == Diamond) 
    val clubs = cards filter (y => y.suit == Club) 
    hearts ++ spades ++ diamonds ++ clubs
  }
  
  val sortByFace : List[Card] = cards.sortWith( _.integerFaceValue < _.integerFaceValue )
  
  val highestValue : Card = sortByFace.reverse head
  
  val sameSuit : Boolean =  sortBySuit.head.suit == sortBySuit.reverse.head.suit
  
  val sameFace : Boolean =  sortByFace.head.faceValue == sortByFace.reverse.head.faceValue
  
  val consecutiveCards : Boolean = {
    
    def isConsecutive(l: List[Int]) : Boolean = {
      l match {
        case x if x.size < 3 => true
        case x => {
          val diff1 = x.tail.head - x.head 
          val diff2 = x.tail.tail.head - x.tail.head
          diff1 == 1 && diff2 == 1 && isConsecutive(x.tail)
        }
      }
    }
    
    isConsecutive(sortByFace map (_.integerFaceValue))
  }
  
  val kinds : List[Kind] = {
    val cardsGroupedByValue : Map[String, List[Card]] = cards.groupBy( _.faceValue) 
    val buff = scala.collection.mutable.ListBuffer[Kind]()
    
    cardsGroupedByValue.foreach{ case (key, value) => buff +=  Kind(key, value.size) }
    
    buff.toList
  }
  
  val types : List[HandType] = {
    List(
	    new RoyalFlush(this),
	    new StraightFlush(this),
	    new FourOfAKind(this),
	    new FullHouse(this),
	    new Flush(this),
	    new Straight(this),
	    new ThreeOfAKind(this),
	    new TwoPairs(this),
	    new OnePair(this)
    ) filter ( _.isHand)
  }

  // Core method - compares 2 hands
  def compareTo(other: Hand): ComparisionResult = {
    (types, other.types) match {
      case (Nil, Nil) => compareCardsByHighestValue(this.sortByFace.reverse, other.sortByFace.reverse)
      case (x, Nil) => Greater
      case (Nil, x) => Smaller
      case (x, y) => {
        val handType: HandType = x head
        val otherHandType: HandType = y.head
        val result = handType.compareTo(otherHandType)
        if (result == Equals) {
          compareCardsByHighestValue(this.sortByFace.reverse, other.sortByFace.reverse)
        } else {
          result
        }
      }
    }
  }

  def compareCardsByHighestValue(a: List[Card], b: List[Card]): ComparisionResult = {
    if (!(a isEmpty) && !(b isEmpty)) {
      val result = a.head.compareTo(b.head)
      if (result == Equals)
        compareCardsByHighestValue(a.tail, b.tail)
      else
        result
    } else {
      Equals
    }
  }
}

case class Kind(faceValue: String, repeat: Int) {
  def < (other:Kind) : Boolean = if(repeat != other.repeat) repeat < other.repeat else faceValue < other.faceValue  
  def == (other:Kind) : Boolean = faceValue == other.faceValue && repeat == other.repeat
  def > (other:Kind) : Boolean =  !(this < other) && !(this == other)
  def compareTo(other: Kind) : ComparisionResult = {
    if(this == other) Equals
    else if(this < other) Smaller
    else Greater
  }
}

trait HandType {
  val hand: Hand
  def rank: Int
  def isHand: Boolean
  
  def rankCompare(other: HandType): ComparisionResult = {
    if (rank == other.rank) Equals
    else if (rank < other.rank) Smaller
    else Greater
  }
  
  def classEquals(other: HandType): Boolean = {
    val type1 = this.getClass
    val type2 = other.getClass
    if (type1 != type2)
      throw new RuntimeException("Can not compare Hand Types of different classes!")
    else true
  }

  def compareTo(other: HandType): ComparisionResult = {
    if (rankCompare(other) != Equals)
      rankCompare(other)
    else {
      classEquals(other)
      Equals
    }
  }
  
}

// Trait for Hand types like - FourOfAKind, ThreeOfAKind...
trait HandTypeWithKind extends HandType {
  val kinds : List[Kind]
  
  override def compareTo(other: HandType): ComparisionResult = {
    if (rankCompare(other) != Equals)
      rankCompare(other)
    else {
      val hand2 = other.asInstanceOf[HandTypeWithKind]

      def loop(a: List[Kind], b: List[Kind]): ComparisionResult = (a, b) match {
        case (Nil, Nil) => Equals
        case (Nil, x) => Smaller
        case (x, Nil) => Greater
        case (x: List[Kind], y: List[Kind]) => {
          val result = x.head.compareTo(y.head)
          if (result == Equals) {
            loop(x.tail, y.tail)
          } else {
            result
          }
        }
      }
      loop(kinds, hand2.kinds)
    }
  }
}

case class RoyalFlush(val hand : Hand) extends HandType {
  val rank = 1
  def isHand: Boolean = hand.sameSuit && hand.sortByFace.map(_.faceValue) == List("10", "J", "Q", "K", "A")
}

case class StraightFlush(val hand : Hand) extends HandType {
  val rank = 2
  def isHand : Boolean = hand.sameSuit && hand.consecutiveCards
}

case class FourOfAKind(val hand: Hand) extends HandTypeWithKind {
  val rank = 3
  val kinds : List[Kind] = hand.kinds.filter(_.repeat == 4)
  val isHand: Boolean = !(kinds isEmpty)
}

case class FullHouse(val hand: Hand) extends HandType {
  val rank = 4
  def isHand: Boolean = {
    val kind3 = hand.kinds.filter(_.repeat == 3)
    val kind2 = hand.kinds.filter(_.repeat == 2)
    !(kind3 isEmpty) && !(kind2 isEmpty)
  }
}

case class Flush(val hand: Hand) extends HandType {
  val rank = 5
  def isHand: Boolean = hand.sameSuit
}

case class Straight(val hand: Hand) extends HandType {
  val rank = 6
  def isHand: Boolean = hand.consecutiveCards
}

case class ThreeOfAKind(val hand: Hand) extends HandTypeWithKind {
  val rank = 7
  val kinds: List[Kind] = hand.kinds.filter(_.repeat == 3)
  val isHand: Boolean = !(kinds isEmpty)
}

case class TwoPairs(val hand: Hand) extends HandTypeWithKind {
  val rank = 8
  val kinds: List[Kind] =  hand.kinds.filter(_.repeat == 2)
  def isHand: Boolean = !(kinds isEmpty) && (kinds.size == 2)
}

case class OnePair(val hand: Hand) extends HandTypeWithKind {
  val rank = 9
  val kinds : List[Kind] = hand.kinds.filter(_.repeat == 1)
  def isHand: Boolean = !(kinds isEmpty) && (kinds.size == 2)
}