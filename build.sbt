val scala2Version = "2.13.16"
val scala3Version = "3.3.6"
val scalaVersions = List(scala2Version, scala3Version)

val catsVersion = "2.13.0"
val disciplineVersion = "2.3.0"
val scalaTestVersion = "3.2.19"

lazy val root = project
  .in(file("."))
  .settings(
    name := "signed-algebra",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    crossScalaVersions := scalaVersions,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "algebra" % catsVersion,
      "org.typelevel" %% "cats-core" % catsVersion,

      "org.typelevel" %% "algebra-laws" % catsVersion % Test,
      "org.typelevel" %% "cats-laws" % catsVersion % Test,
      "org.scalactic" %% "scalactic" % scalaTestVersion % Test,
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
      "org.typelevel" %% "discipline-scalatest" % disciplineVersion % Test
    )
  )
