object OptionStudy {

  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(s) => Some(f(s))
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(s) => s
      case None => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this map f getOrElse None
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

}

