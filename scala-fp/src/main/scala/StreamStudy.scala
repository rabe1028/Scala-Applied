// Challenge 7-2

trait StreamStudy[+A] {

  def headOption: Option[A] = this match {
    case EmptyStream => None
    case Cons(h, t) => Some(h())
  }

  def tail: StreamStudy[A] = this match {
    case EmptyStream => EmptyStream
    case Cons(h, t) => t()
  }
}

case object EmptyStream extends StreamStudy[Nothing]

case class Cons[+A](h: () => A, t: () => StreamStudy[A]) extends StreamStudy[A]

object StreamStudy {

  // Challenge 7-3
  def cons[A](h: => A, t: => StreamStudy[A]): StreamStudy[A] = {
    lazy val cache_h = h
    lazy val cache_t = t
    Cons(
      () => {
        cache_h
      }, () => {
        cache_t
      })
  }

  def empty[A]: StreamStudy[A] = EmptyStream

}