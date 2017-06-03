import shapeless.ops.hlist.Tupler
import shapeless.{::, HList, HNil}
import scala.collection.GenTraversable
import shapeless._, ops.traversable.FromTraversable

class FromListToCaseClass[T] {
  def apply[R <: HList](l: GenTraversable[_])
                       (implicit gen: Generic.Aux[T, R], tl: FromTraversable[R]): Option[T] =
    tl(l).map(gen.from)
}
def fromListToCaseClass[T] = new FromListToCaseClass[T]

case class IntWrapper(i: Int)

val z = fromListToCaseClass[IntWrapper](List("hi"))
z

trait Rule[A] {
  def returnSomething: A
}

case class Something[A](a: A) extends Rule[A] {
  override def returnSomething: A = a
}

class ConcatedRules[H <: HList](hs: H)(implicit val tupler: Tupler[H])  {
  def tupled = hs.tupled(tupler)
}

case class Two(a: Int, b: Int)

val two: (Int, Int) => Two = Two

case class ConcatRules2[A, B](a: A, b: B) {
  def toTuple: (A, B) = (a, b)
  def toCaseClass[C](converter: (A, B) => C) = converter(a, b)
}

ConcatRules2(1, 2).toCaseClass(Two)

case class ConcatRule[A, B](left: A, right: B) {
  def toConcatedRules = new ConcatedRules(left :: right :: HNil)
}

object Implicits {
  implicit class ConcatRules[A](a: A) {
    def *[B](b: Rule[B]) = {
      ConcatRule(a,b)
    }
  }
}

import Implicits._

val r0 = Something(123)
val r1 = Something("abc")
val r2 = Something("def")

val x = (r0 * r1).toConcatedRules.tupled

