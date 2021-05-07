import org.scalatest.{DiagrammedAssertions, FlatSpec}

class NumberFormatterSpec extends FlatSpec with DiagrammedAssertions {
  import NumberFormatter.format

  it should "3桁の整数を入れて問題なく動作する" in {
    assert("100" === format(100))
  }

  it should "負の３桁の値を入れても問題なく動作する" in {
    assert("-100" === format(-100))
  }
}
