name := "Domino"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= {
  val akkaV = "2.3.9"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "org.scalatest" %% "scalatest" % "2.2.5" % "test"
  )
}