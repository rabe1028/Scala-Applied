import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

// Challenge 10-3

object MacroChallenge {
  def asserteq(left: Int, right: Int, msg: Any) = macro asserteqImpl

  def asserteqImpl(c: Context)
    (left: c.Expr[Int], right: c.Expr[Int], msg: c.Expr[Any]) : c.Expr[Unit] = {
    import c.universe._
    reify {
      assert(left.splice.equals( right.splice) , msg.splice)
    }
  }
}
