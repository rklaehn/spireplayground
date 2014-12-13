// import the machinery for operators like ===
import spire.syntax.all._

object EqTest extends App {
  // redefine the equality for double, just in this scope, to mean fuzzy equality
  implicit object FuzzyDoubleEq extends spire.algebra.Eq[Double] {
    def eqv(a:Double, b:Double) = (a-b).abs < 1e-5
  }

  require(0.0 === 0.000001)

  // import automatic generation of type class instances for tuples based on type classes for scalars
  import spire.std.tuples._
  require((0.0, 0.0) === (0.000001, 0.0)) // works also for tuples containing doubles

  // import automatic generation of type class instances for arrays based on type classes for scalars
  import spire.std.array._
  require(Array(0.0,1.0) === Array(0.000001, 1.0)) // and for arrays of doubles

  import spire.std.seq._
  require(Seq(1.0, 0.0) === Seq(1.000000001, 0.0))
}