ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "zio-optics-examples",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"        % "1.0.10",
      "dev.zio" %% "zio-optics" % "0.1.0"
    )
  )
