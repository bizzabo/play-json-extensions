import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform.{ScalariformKeys, autoImport}

val projectName = "play-json-extensions"
lazy val root = Project(id = projectName, base = file("."))

version := "0.42.1-SNAPSHOT"
organization := "ai.x"
name := projectName
scalaVersion := "2.12.10"
crossScalaVersions := Seq("2.12.10", "2.13.1")
useGpg := true
credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")
description := "Additional type classes for the play-json serialization library"
organizationName := "x.ai - Magically schedule meetings"

val ghProject = "xdotai/"+projectName
val ghUrl = url( "https://github.com/" + ghProject )

homepage := Some( ghUrl )
startYear := Some(2015)
licenses += (
  "Two-clause BSD-style license",
  url( ghUrl + "/blob/master/LICENSE.txt" )
)
scmInfo := Some(
  ScmInfo( ghUrl, "git@github.com:" + ghProject + ".git" )
)
developers := List(
  Developer("cvogt", "Jan Christopher Vogt", "@cvogt", url("https://github.com/cvogt"))
)

libraryDependencies ++=   Seq(
  "com.typesafe.play" %% "play-json" % "2.8.1",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

scalacOptions ++= Seq(
  "-feature", "-deprecation", "-unchecked",
  "-language:experimental.macros",
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) => "-Ywarn-unused-import"
    case _ => "-Ywarn-unused:imports"
  },
  "-Xfatal-warnings"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oFD")
parallelExecution := false // <- until TMap thread-safety issues are resolved

scalacOptions in (Compile, doc) ++= Seq(
  "-doc-title", name.value,
  "-doc-version", version.value,
  "-doc-footer", projectName+" is developed by x.ai.",
  "-sourcepath", (sourceDirectory in Compile).value.getPath, // needed for scaladoc to strip the location of the linked source path
  "-doc-source-url", ghUrl+"/blob/"+version.value+"/src/main€{FILE_PATH}.scala",
  "-implicits",
  "-diagrams", // requires graphviz
  "-groups"
)

publishTo := sonatypePublishTo.value

publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := { _ => false }

scalariformPreferences := scalariformPreferences.value
      .setPreference(AlignParameters, true)
      .setPreference(AlignArguments, true)
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
      .setPreference(SpaceInsideParentheses, true)
      .setPreference(SpacesWithinPatternBinders, true)
      .setPreference(SpacesAroundMultiImports, true)
      .setPreference(DanglingCloseParenthesis, Preserve)
.setPreference(DoubleIndentConstructorArguments, true)

