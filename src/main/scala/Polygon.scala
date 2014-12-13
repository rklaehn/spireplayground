import spire.algebra._
import spire.math.Rational
import spire.std.any._
import spire.syntax.all._

object Polygon extends App {

  implicit class LineSegmentOps[T](private val value: LineSegment[T]) extends AnyVal {
    def xmin(implicit order: Order[T]) = value.a.x min value.b.x
    def xmax(implicit order: Order[T]) = value.a.x max value.b.x
    def ymin(implicit order: Order[T]) = value.a.y min value.b.y    
    def ymax(implicit order: Order[T]) = value.a.y max value.b.y    
    def dx(implicit f: Field[T]) = value.a.x - value.b.x
    def dy(implicit f: Field[T]) = value.a.y - value.b.y
  }

  case class Vertex[T](x: T, y: T)

  case class LineSegment[T](a: Vertex[T], b: Vertex[T])

  def pointOnLine[T: Field, Order](a:LineSegment[T], b:Vertex[T]) : Boolean = {
    ???
  }

  def intersection[T: Field: Order](a: LineSegment[T], b: LineSegment[T]): Option[Vertex[T]] = {
    if (a.xmax < b.xmin || b.xmax < a.xmin)
      None
    else if (a.ymax < b.ymin || b.ymax < a.ymin)
      None
    else {
      // Segment 1: (x11,y11) to (x12, y12) //Segment 2: (x21,y21) to (x22, y22)
      val a_dx = a.dx
      val b_dx = b.dx
      val a_dy = a.dy
      val b_dy = b.dy

      val x = (a.a.x * a_dy * b_dx - b.a.x * b_dy * a_dx + b.a.y * a_dx * b_dx - a.a.y * a_dx * b_dx) / (a_dy * b_dx - b_dy * a_dx)
      val y = (a.a.y * a_dx * b_dy - b.a.y * b_dx * a_dy + b.a.x * a_dy * b_dy - a.a.x * a_dy * b_dy) / (a_dx * b_dy - b_dx * a_dy)
      Some(Vertex(x, y))
    }
  }

  val a = LineSegment(Vertex[Rational](0, 0.5), Vertex[Rational](1, 1))
  val b = LineSegment(Vertex[Rational](0, 0), Vertex[Rational](1, 2))
  val i = intersection(a, b)
  println(i)
}
