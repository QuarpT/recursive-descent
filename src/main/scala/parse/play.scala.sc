import shapeless.ops.hlist.{IsHCons, Prepend}
import shapeless.{::, HList, HNil}

import scala.collection.immutable.Nil
case class User(name: String)

val demo: ::[Int, ::[String, ::[User, HNil]]] = 42 :: "Hello" :: User("Julien") :: HNil

val x: ::[Int, ::[String, HNil]] = 5 :: "Hi" :: HNil
val y: ::[Int, ::[String, HNil]] = 2 :: "Hi" :: HNil

val z: ::[Int, ::[String, ::[Int, ::[String, HNil]]]] = x ++ y

case class MyHList[A <: HList](list1: A) {
  def merge[B <: HList](list2: B)(implicit prepend : Prepend[A, B]): MyHList[Prepend[A, B]#Out] =MyHList {
    list1 ++ list2
  }
}
val another = MyHList(123 :: HNil).merge("hi" :: HNil)


def fib(x: Int): Int = {
  if (x < 2) 1
  else {
    lazy val a = fib(x - 1)
    lazy val b = fib(x - 2)
    a + b
  }
}

fib(5)

