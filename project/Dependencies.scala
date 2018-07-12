import sbt._

object Dependencies {

  val CatsVersion = "1.0.1"
  val ScalaXmlVersion = "1.1.0"

  lazy val cats = Seq(
    "org.typelevel" %% "cats-core" % CatsVersion,
    "org.typelevel" %% "cats-effect" % "1.0.0-RC2")

  lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" %  ScalaXmlVersion

  lazy val dependencies: Seq[ModuleID] = cats ++ Seq(
    scalaXml
  )

}
