import org.scalatest.{DiagrammedAssertions, FlatSpec}

class StackSpec extends FlatSpec with DiagrammedAssertions  {
  "Stack" should "逆順で取り出せる" in {
    val stack = Stack().push(1).push(2).push(3)
    assert(stack.isEmpty === false)

    val (pop1, stack1) = stack.pop
    assert(3 === pop1)
    val (pop2, stack2) = stack1.pop
    assert(2 === pop2)
    val (pop3, stack3) = stack2.pop
    assert(pop3 === 1)

    assert(stack3.isEmpty)
  }

  "Empty Stack" should "popで例外発生" in {
    intercept[IllegalArgumentException] {
      Stack().pop
    }
  }
}
