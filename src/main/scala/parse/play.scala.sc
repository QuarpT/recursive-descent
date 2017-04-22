import shapeless.{ ::, HList, HNil }
case class User(name: String)

val demo: ::[Int, ::[String, ::[User, HNil]]] = 42 :: "Hello" :: User("Julien") :: HNil
val x: ::[Int, ::[Int, ::[String, ::[User, HNil]]]] = 5 :: demo
