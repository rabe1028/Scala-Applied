import scala.util.Try

object UnapplyStudy extends App {

  class User(private val name: String, private val age: Int)

  object User {
    def unapply(obj: Any): Option[(String, Int)] =
      obj match {
        case user: User =>
          Some((user.name, user.age))
        case str: String =>
          val strs = str.split("@")
          val name = strs.headOption
          val age = Try {
            strs.tail.headOption.map(_.toInt)
          }.toOption.flatten
          (name, age) match {
            case (Some(n), Some(a)) => Some((n, a))
            case _ => None
          }
        case _ =>
          None
      }
  }

  def printPatternMatched(obj: AnyRef): Unit = {
    obj match {
      case User(name, age) => println(s"Name: ${name}, Age: ${age}")
      case _ => println("can't extract")
    }
  }
}