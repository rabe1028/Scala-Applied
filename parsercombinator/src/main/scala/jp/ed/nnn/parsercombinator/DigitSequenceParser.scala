package jp.ed.nnn.parsercombinator

// Challenge 11-2
case class DigitSequence(seq: Seq[String])

case object DigitSequenceParser extends MyFirstCombinator {
  def digit: Parser[String] = oneOf('0' to '9')

  def apply(input: String): ParseResult[DigitSequence] = map(rep(digit), {
    t: List[String] => DigitSequence(t)
  })(input)
}
