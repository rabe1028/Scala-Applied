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
      pointOpt <- results.get(name).toRight(StudentNotFound).right
      point <- pointOpt.toRight(ResultNotFound).right
    } yield Point(point)).merge
  }

  println(find("taro")) // Point(90)
  println(find("jiro")) // ResultNotFound
  println(find("saburo")) // StudentNotFound
}
