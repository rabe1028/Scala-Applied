import scala.reflect.runtime.universe._

object ReflectionStudy {

  def main(): Unit = {
    println("========= from class =========")
    println(typeTag[Option[_]].tpe.decls)

    val list = List(1,2,3)

    def getTypeTagFromList[T: TypeTag](list: List[T]) : TypeTag[T] = typeTag[T]

    println("========= from type parameter =========")
    println(getTypeTagFromList(list).tpe.decls)
  }

}