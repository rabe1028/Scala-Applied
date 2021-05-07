import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object MacroStudy {
  def accessor(obj: Any, property: String): Any = macro impl_accessor

  def impl_accessor(c: Context)(obj: c.Expr[Any], property: c.Expr[String]): c.universe.Select = {
    import c.universe._
    val Expr(Literal(Constant(propString: String))) = property
    Select(obj.tree, TermName(propString))
  }
}