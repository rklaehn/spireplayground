import java.text.ParseException

import spire.math.Rational

import scala.collection.immutable.TreeMap
import spire.algebra._

import scala.collection.mutable.ArrayBuffer

final class UOM private(private val underlying: TreeMap[String, Int]) extends AnyVal { lhs =>

  override def toString: String = UOM.Formatter(underlying, unicode = true)
}

object UOM {

  implicit object algebra extends Field[UOM] with NRoot[UOM] with Trig[UOM] with Eq[UOM] with Order[UOM] {

    override def eqv(x: UOM, y: UOM): Boolean = x == y

    override def one: UOM = dimensionless

    override def times(x: UOM, y: UOM): UOM = wrap(combine(x.underlying, y.underlying, _ + _))

    override def div(x: UOM, y: UOM): UOM = wrap(combine(x.underlying, y.underlying, _ - _))

    override def negate(x: UOM): UOM = x

    override def e: UOM = dimensionless

    override def atan(a: UOM): UOM = nodim(a, "atan")

    override def acos(a: UOM): UOM = nodim(a, "acos")

    override def tanh(x: UOM): UOM = nodim(x, "tanh")

    override def log(a: UOM): UOM = nodim(a, "log")

    override def cosh(x: UOM): UOM = nodim(x, "cosh")

    override def tan(a: UOM): UOM = nodim(a, "tan")

    override def cos(a: UOM): UOM = nodim(a, "cos")

    override def exp(a: UOM): UOM = nodim(a, "exp")

    override def expm1(a: UOM): UOM = nodim(a, "expm1")

    override def asin(a: UOM): UOM = nodim(a, "asin")

    override def pi: UOM = dimensionless

    override def log1p(a: UOM): UOM = nodim(a, "log1p")

    override def toRadians(a: UOM): UOM = nodim(a, "toRadians")

    override def sin(a: UOM): UOM = nodim(a, "sin")

    override def atan2(y: UOM, x: UOM): UOM = same(x, y, "atan2")

    override def toDegrees(a: UOM): UOM = nodim(a, "toDegrees")

    override def sinh(x: UOM): UOM = nodim(x, "sinh")

    override def zero: UOM = dimensionless

    override def quot(a: UOM, b: UOM): UOM = plus(a,b)

    override def gcd(a: UOM, b: UOM): UOM = same(a,b,"gcd")

    override def mod(a: UOM, b: UOM): UOM = div(a,b)

    override def plus(x: UOM, y: UOM): UOM = same(x,y,"sum")

    override def compare(x: UOM, y: UOM): Int = { same(x,y,"compare"); 0 }

    override def nroot(a: UOM, n: Int): UOM = {
      wrap(a.underlying.map {
        case (unit,exponent) =>
          if((exponent % n) != 0)
            throw new IllegalArgumentException(
              s"Trying to take the ${n}th root of ${UOM(unit, exponent)}, but fractional dimensions not allowed!")
          (unit, exponent / n)
      })
    }

    override def fpow(a: UOM, b: UOM): UOM = {
      if(b != dimensionless)
        throw new IllegalArgumentException(s"Exponent must be dimensionless $a^$b.")
      a
    }

    private def same(x: UOM, y: UOM, text: String): UOM = {
      if (x != y)
        throw new IllegalArgumentException(s"Attempt to $text unit $x and $y")
      else
        x
    }

    private def nodim(x: UOM, text: String): UOM = {
      if (x != dimensionless)
        throw new IllegalArgumentException(s"Not dimensionless in operation $text")
      else
        x
    }
  }

  val dimensionless: UOM = new UOM(TreeMap.empty)

  def parse(text: String) : UOM = Formatter.parse(text)

  private object Formatter {

    import SuperscriptUtil._

    val dot = '\u00B7'

    val pattern = """([a-zA-Z_]+)([^a-zA-Z_]*)""".r

    def parseExponent(text: String) : Int = {
      val text1 = text.trim.stripPrefix("^").trim.stripPrefix("(").stripSuffix(")").trim
      if(text1 == "") 1 else text1.toInt
    }

    def splitOnStart(text:String, f:Char => Boolean) = {
      val starts = 0 +: text.indices.tail.filter(i => !f(text(i-1)) && f(text(i))) :+ text.length
      starts.zip(starts.tail).map {
        case (i0,i1) => text.substring(i0,i1)
      }
    }

    def parse(text: String) : UOM = {
      import spire.syntax.all._
      val p = """([A-Za-z_]+)([^A-Za-z_]*?)(\*|/|\u00B7)?\s*""".r
      val dimensionless = """\s*1?\s*""".r
      val oneDiv = """\s*1\s*/(.*)""".r
      def parse0(text:String, s:Int) = {
        var sign = s
        splitOnStart(text, Character.isJavaIdentifierStart).map {
          case p(unit, exponent, p) =>
            val sign0 = sign
            sign = if(p == "/") -1 else 1
            UOM(unit, parseExponent(exponent) * sign0)
        }
      }
      val elements = text match {
        case dimensionless() => Seq.empty
        case oneDiv(text) => parse0(text, -1)
        case text => parse0(text, 1)
      }
      /*
      val elements = text.split(Array(dot, '*', ' '))
        .map(_.trim)
        .filterNot(_.isEmpty)
        .map({
          case pattern(unit, exponent) =>
            UOM(unit, parseExponent(exponent.map(removeSuperscript)))
      })*/
      (UOM.dimensionless /: elements)(_ * _)
    }

    def apply(a: TreeMap[String, Int], unicode: Boolean = false): String = {
      /*
      val parts = a.toArray
      parts.indices.map { i =>
        val (unit, exponent) = parts(i)
        val prefix =
          if(i==0) {
            if (exponent < 0) "1/" else ""
          } else {
            if (exponent < 0) "/" else dot
          }
        val multi = if(exponent.abs != 1) exponent.abs.toString.map(superscript) else ""
        s"$prefix$unit$multi"
      }.mkString
      */
      if (unicode) {
        a.map({
          case (unit, exponent) =>
            if (exponent != 1)
              unit + exponent.toString.map(superscript)
            else
              unit
        }).mkString(dot.toString)
      } else {
        a.map({
          case (unit, exponent) =>
            if (exponent != 1)
              s"$unit^$exponent"
            else
              unit
        }).mkString(" ")
      }
    }

  }

  private def combine(a: TreeMap[String, Int], b: TreeMap[String, Int], op: (Int, Int) => Int): TreeMap[String, Int] = {
    (a /: b) {
      case (m, (k, y)) =>
        val x = m.getOrElse(k, 0)
        val r = op(x, y)
        if (r != 0)
          m.updated(k, r)
        else
          m - k
    }
  }

  private def wrap(value: TreeMap[String, Int]) = new UOM(value)

  def apply(name: String, exponent: Int) =
    if (exponent != 0)
      wrap(TreeMap(Symbol(name).name -> exponent))
    else
      dimensionless

  def apply(text: String): UOM =
    parse(text)
}

object SuperscriptUtil {

  def toSuperscriptString(value:Double) = {
    val text = value.toString.toLowerCase
    val i = text.indexOf('e')
    if(i<0) text else {
      val dot = '\u00B7'
      text.take(i) + dot + "10" + text.drop(i+1).map(superscript)
    }
  }

  private val mapping = Array(
    '0' -> '\u2070',
    '1' -> '\u00B9',
    '2' -> '\u00B2',
    '3' -> '\u00B3',
    '4' -> '\u2074',
    '5' -> '\u2075',
    '6' -> '\u2076',
    '7' -> '\u2077',
    '8' -> '\u2078',
    '9' -> '\u2079',
    '+' -> '\u207A',
    '-' -> '\u207B',
    '=' -> '\u207C',
    '(' -> '\u207D',
    ')' -> '\u207E',
    'n' -> '\u207F'
  )

  val superscript : (Char => Char) = Map(mapping:_*).withDefault(identity)

  val removeSuperscript : (Char => Char) = Map(mapping.map(_.swap):_*).withDefault(identity)
}
