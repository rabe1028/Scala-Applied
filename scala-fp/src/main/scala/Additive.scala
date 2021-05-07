object ChallengeAdditive {

  trait Additive[A] {
    def plus(a: A, b: A): A

    def zero: A
  }

  // Challenge 8-3
  case class Polynomial(coefficient: List[Double])

  object Polynomial {
    implicit object PolynomialAdditive extends Additive[Polynomial] {
      def plus(a: Polynomial, b: Polynomial) : Polynomial = {

        def plusInternal(_a: Polynomial, _b: Polynomial): Polynomial = {
          assert(_a.coefficient.length < _b.coefficient.length)
          val padding_a: List[Double] = _a.coefficient ++ List.fill(_b.coefficient.length - _a.coefficient.length)(0.0)
          // assert(padding_a.length == _b.coefficient.length)
          Polynomial(padding_a.zip(_b.coefficient).map(n => n._1 + n._2))
        }

        if(b.coefficient.length < a.coefficient.length) {
          plusInternal(b, a)
        } else {
          plusInternal(a, b)
        }
      }

      def zero: Polynomial = Polynomial(List(0))
    }
  }

  def sum[A](lst: List[A])(implicit m: Additive[A]): A = lst.foldLeft(m.zero)((x, y) => m.plus(x, y))
}
