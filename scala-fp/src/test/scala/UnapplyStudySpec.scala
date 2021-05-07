import org.scalatest.{DiagrammedAssertions, FlatSpec}
import UnapplyStudy._

class UnapplyStudySpec extends FlatSpec with DiagrammedAssertions  {
  it should "文字でもUserでもパターンマッチできる" in {
    printPatternMatched(new User("Taro", 17))
    printPatternMatched("Jiro@16")
  }
}
