// Challenge 10-1

object UnapplyChallenge {
  class Book(private val title: String)

  object Book {
    def unapply(book: Book): Option[String] = {
      Some(book.title)
    }
  }
}
