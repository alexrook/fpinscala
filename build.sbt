ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .aggregate(exercises)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(
    name := "exercises"
  )

