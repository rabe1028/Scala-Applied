import org.scalatest.{DiagrammedAssertions, FlatSpec}

class MacroStudySpec  extends FlatSpec with DiagrammedAssertions {
  it should "構文から、指定した引数を取り出す" in {
    case class User(name: String)
    val user = User("taro")
    assert(MacroStudy.accessor(user, "name") === "taro")
  }
}
