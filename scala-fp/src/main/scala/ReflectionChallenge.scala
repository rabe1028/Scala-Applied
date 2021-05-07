// Challenge 10-2
import scala.reflect.runtime.universe._

class Issue(private val title: String) {
  private def printTitle(): Unit = println(title)
}

object ReflectionChallenge {
  val issue = new Issue("不具合1")

  def run(): Unit = {
    // TypeTag[Issue].tpe.decl
    val m = runtimeMirror(getClass.getClassLoader)
    val classIssue = typeOf[Issue].typeSymbol.asClass

    val printTitleTermSymbol = typeOf[Issue].decl(TermName("printTitle")).asMethod

    val im = m.reflect(issue)
    val printTitleMirror = im.reflectMethod(printTitleTermSymbol)

    printTitleMirror()
  }
}