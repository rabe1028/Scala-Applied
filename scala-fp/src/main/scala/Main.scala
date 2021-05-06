import scala.annotation.tailrec

object Main {

  @tailrec
  def series(n: Int, acc: Int): Int = {
    if (n == 0) {
      acc
    } else {
      series(n - 1, acc + n)
    }
  }

  // Challenge 1
  @tailrec
  def fact(n: Int, acc: Int): Int = {
    if (n <= 1) {
      acc
    } else {
      fact(n - 1, acc * n)
    }
  }

  // Challenge 2
  case class Switch(var isOn: Boolean)

  def toggle(switch: Switch): Switch = {
    Switch(!switch.isOn)
  }

  // Challenge 3
  def twice(f: Int => Int) = {
    (x: Int) => f(f(x))
  }

}
