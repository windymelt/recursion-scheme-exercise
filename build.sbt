val sv = "2.13.8"

addCompilerPlugin(
  "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "recursion-scheme-exercise",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := sv,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "io.higherkindness" %% "droste-core" % "0.9.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
