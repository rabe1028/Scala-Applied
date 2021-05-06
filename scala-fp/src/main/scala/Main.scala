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

  // Challenge 1-1
  @tailrec
  def fact(n: Int, acc: Int): Int = {
    if (n <= 1) {
      acc
    } else {
      fact(n - 1, acc * n)
    }
  }

  // Challenge 1-2
  case class Switch(var isOn: Boolean)

  def toggle(switch: Switch): Switch = {
    Switch(!switch.isOn)
  }

  // Challenge 1-3
  def twice(f: Int => Int) = {
    (x: Int) => f(f(x))
  }

  def search[T](seq: Seq[T])(f: T => Boolean) : Boolean = {
    def searchRec(i: Int) : Boolean = {
      if(seq.length == i) false
      else if (f(seq(i))) true
      else searchRec(i+1)
    }
    searchRec(0)
  }

  // Challenge 2-3
  def isSorted[E](sortedSeq: Seq[E])(ordered: (E, E) => Boolean): Boolean = {
    def isSortedRec(i: Int): Boolean = {
      // 末尾に達した時、もしくは、要素がない時
      if (sortedSeq.length == i + 1 || sortedSeq.isEmpty) true
      else if (ordered(sortedSeq(i), sortedSeq(i + 1))) isSortedRec(i + 1)
      else false
    }
    isSortedRec(0)
  }

}
