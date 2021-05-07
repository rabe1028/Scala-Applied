import scala.{:+, ::}
import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, TreeMap}
import scala.util.Try
import scala.util.control.NonFatal

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

  // Challenge 3-1
  def optionHandling(): Option[Int] = {
    val v1: Option[Int] = Some(2)
    val v2: Option[Int] = Some(3)
    val v3: Option[Int] = Some(5)
    val v4: Option[Int] = Some(7)
    val v5: Option[Int] = Some(11)

    for {
      i1 <- v1
      i2 <- v2
      i3 <- v3
      i4 <- v4
      i5 <- v5
    } yield i1 * i2 * i3 * i4 * i5
  }

  // Challenge 3-2
  def optionalFunction(): Option[Int => Int] = {
    val f1: Option[Int => Int] = Some((x) => x * 2)
    val f2: Option[Int => Int] = Some((x) => x + 10)
    val f3: Option[Int => Int] = Some((x) => x / 3)
    for {
      w1 <- f1
      w2 <- f2
      w3 <- f3
    } yield w1 andThen w2 andThen w3
  }

  // Challenge 4-1
  def createString(size: Int): Try[String] = {
    Try {
      require(size >= 0, "sizeはゼロ以上である必要があります")
      (for (i <- 0 until size) yield "a").mkString
    }
  }

  // Challenge 5-1
  def benchmark(tag: String = "")(f: => Unit): Unit = {
    val begin = System.currentTimeMillis()
    f
    val end = System.currentTimeMillis()
    val formatter = java.text.NumberFormat.getNumberInstance()
    println(s"time: ${formatter.format(end - begin)} ミリ秒 (${tag}) ")
  }

  def benchmarkAppendPerformance(): Unit = {
    println("末尾追加")
    benchmark("List") {
      var result = (0 until 10000).foldLeft(List.empty[Int]) {(acc: List[Int], i: Int) =>
        acc :+ i
      }
    }

    benchmark("配列") {
      var result = (0 until 10000).foldLeft(Array.empty[Int]) {(acc: Array[Int], i: Int) =>
        acc :+ i
      }
    }

    println("先頭追加")
    benchmark("List") {
      var result = (0 until 10000).foldLeft(List.empty[Int]) {(acc: List[Int], i: Int) =>
        i :: acc
      }
    }

    benchmark("配列") {
      var result = (0 until 10000).foldLeft(Array.empty[Int]) {(acc: Array[Int], i: Int) =>
        i +: acc
      }
    }
  }

  // Challenge 5-2
  def benchmarkNano(tag: String = "")(f: => Unit) = {
    val begin = System.nanoTime()
    f
    val end = System.nanoTime()
    val formatter = java.text.NumberFormat.getNumberInstance()
    println(s"time: ${formatter.format(end - begin)} ナノ秒 (${tag})")
  }

  def benchmarkIndexing(): Unit = {
    val size = 10000000
    val list = (1 to size).toList
    benchmarkNano("List") {
      list(10000000 - 1)
    }
    val arr  = (1 to size).toArray
    benchmarkNano("Array") {
      arr(10000000 - 1)
    }
  }

  // Challenge 5-3
  def benchmarkMapSpec(): Unit = {
    val size = 100000

    var arr: Array[Int] = Array.empty
    benchmarkNano("Array, Instance") {
      arr = (1 to size).toArray
    }
    benchmarkNano("Array, Index") {
      arr(size - 1)
    }

    var hashmap: HashMap[Int, Int] = HashMap.empty
    benchmarkNano("Hashmap, Instance") {
      hashmap = HashMap((1 to size).map(i => i -> i): _*)
    }

    benchmarkNano("Hashmap, Index") {
      hashmap(size - 1)
    }

    var treemap: TreeMap[Int, Int] = TreeMap.empty
    benchmarkNano("Treemap, Instance") {
      treemap = TreeMap((1 to size).map(i => i -> i): _*)
    }

    benchmarkNano("Treemap, Index") {
      treemap(size - 1)
    }

  }

  // Challenge 6-1
  def filter[T](list: List[T]) (f: T => Boolean) : List[T] = {
    list.foldLeft(List.empty[T]) {(acc, item) => {
      if(f(item)) acc :+ item else acc
    }}
  }

  // Challenge 6-2
  def flatten(list: List[_]): List[Any] = list match {
    case Nil => Nil
    case (x: List[_]) :: tail => flatten(x) ++ flatten(tail)
    case x::tail => x :: flatten(tail)
  }

  // Challenge 6-3
  def split[A](n: Int, list: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitRec(k: Int, acc: (List[A], List[A])): (List[A], List[A]) = {
      if(k == 0) acc
      else if (acc._2.nonEmpty) {
        val x +: xs = acc._2
        splitRec(k - 1, (acc._1 :+ x, xs) )
      } else acc
    }

    splitRec(n, (List.empty[A], list))
  }

  // Challenge 7-1
  def calculateTribonacchi(): Unit = {
    // エラーを吐くので、コメントアウト
//    val tribs: Stream[Int] = 0 #:: 0 #:: 1 #:: tribs.zip(tribs.tail).zip(tribs.tail.tail).map { n => n._1._1 + n._1._2 + n._2 }
  }

  // Challenge 8-1
  implicit class TwiceString(val string: String) {
    def twice: String = string + string
  }

  // Challenge 8-2
  implicit class SplitList[A](val l : List[A]) {
    def split(n: Int): (List[A], List[A]) = {
      @tailrec
      def splitRec(k: Int, acc: (List[A], List[A])): (List[A], List[A]) = {
        if(k == 0) acc
        else if (acc._2.nonEmpty) {
          val x +: xs = acc._2
          splitRec(k - 1, (acc._1 :+ x, xs) )
        } else acc
      }

      splitRec(n, (List.empty[A], l))
    }
  }

  // Challenge 8-3

}
