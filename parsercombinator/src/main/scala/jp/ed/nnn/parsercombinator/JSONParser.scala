package jp.ed.nnn.parsercombinator

// Challenge 12-3

object JSONParser extends Combinator {

  def obj: Parser[Map[String, Any]]  =
    ss("{") ~> repsep(member, ss(",")) <~ ss("}") <~ spacing ^^ { Map() ++ _ }

  def arr: Parser[List[Any]] =
    ss("[") ~> repsep(value, ss(",")) <~ ss("]") <~ spacing

  def member: Parser[(String, Any)] =
    (stringLiteral <~ spacing) ~ ss(":") ~ value <~ spacing  ^^ { t => (t._1._1, t._2) }

  def value: Parser[Any] =
    obj  |
      arr  |
      stringLiteral <~ spacing |
      (floatingPointNumber <~ spacing ^^ { _.toDouble })  |
      ss("null") ^^  { _ => null } |
      ss("true") ^^  { _ => true } |
      ss("false") ^^  { _ => false }

  def apply(input: String): Any = value(input)

}