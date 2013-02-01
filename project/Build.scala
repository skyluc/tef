import sbt._
import Keys._

object AppBuild extends Build {

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := "org.skyluc",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := "2.10.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"
  )

  lazy val tef = Project ("tef", file("."), settings = buildSettings) 

}
