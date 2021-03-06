import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.devcycle",
      scalaVersion := "2.12.3",
      version      := "0.1.0"
    )),
    name := "my-scala-project",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
  )
