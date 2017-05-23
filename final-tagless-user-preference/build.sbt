name := "final-tagless-user-preference"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ypartial-unification"
)

val circeVersion = "0.9.0"

libraryDependencies ++= Seq(
  "org.typelevel"  %% "cats-core"  % "1.0.1",
  "com.chuusai"    %% "shapeless"  % "2.3.3",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
) ++ Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-optics"
).map(_ % circeVersion)

resolvers += Resolver.sonatypeRepo("releases")

// format: off
testOptions in Test += Tests.Argument(
  TestFrameworks.ScalaCheck,
  "-maxSize", "10",
  "-minSuccessfulTests", "100",
  "-maxDiscardRatio", "10.0",
  "-workers", "1",
  "-verbosity", "1"
)
// format: on
