name := "parsing"

version := "1.0"

scalaVersion := "2.11.10"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2"
)