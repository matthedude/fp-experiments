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

libraryDependencies ++= Seq(
  "org.typelevel"  %% "cats-core"  % "1.0.1",
  "com.chuusai"    %% "shapeless"  % "2.3.3",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
) ++ Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-refined",
  "io.circe" %% "circe-optics"
).map(_ % "0.9.0") ++ Seq(
  "eu.timepit" %% "refined",
  "eu.timepit" %% "refined-scalacheck"
).map(_ % "0.8.4")

resolvers += Resolver.sonatypeRepo("releases")
