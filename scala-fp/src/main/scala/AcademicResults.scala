object AcademicResults extends  App {

  private[this] val results = Map(
    "taro" -> Some(90),
    "jiro" -> None
  )

  sealed trait Result
  case class Point(point: Int) extends Result
  sealed trait Error extends Result
  case object StudentNotFound extends Error
  case object ResultNotFound extends Error

  def find(name: String): Result = {
    (for {
      pointOpt <- results.get(name) match {
        case Some(v) => Right(v)
        case None => Left(StudentNotFound)
      }
      point <- pointOpt match {
        case Some(point) => Right(point)
        case None => Left(ResultNotFound)
      }
    } yield Point(point)).merge
  }

  println(find("taro")) // Point(90)
  println(find("jiro")) // ResultNotFound
  println(find("saburo")) // StudentNotFound
}
