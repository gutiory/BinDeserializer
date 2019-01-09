import sbt._

object Dependencies {

  val CatsVersion = "1.0.1"
  val ScalaXmlVersion = "1.1.0"

  lazy val cats = Seq(
    "org.typelevel" %% "cats-core" % CatsVersion,
    "org.typelevel" %% "cats-effect" % "1.0.0-RC2")

  lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" %  ScalaXmlVersion

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val scalaMock = "org.scalamock" %% "scalamock" % "4.1.0"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"


  lazy val dependencies: Seq[ModuleID] = cats ++ Seq(
    scalaXml,
    scalaTest % Test,
    scalaMock % Test,
    scalaCheck % Test
  )

}
