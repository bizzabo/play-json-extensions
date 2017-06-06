val projectName = "play-json-extensions"
lazy val root = Project(id = projectName, base = file("."))

version := "0.9.0"
organization := "ai.x"
name := projectName
scalaVersion := "2.11.11"
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
  "com.typesafe.play" %% "play-json" % "2.5.15",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "compile",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

scalacOptions ++= Seq(
  "-feature", "-deprecation", "-unchecked",
  "-language:experimental.macros",
  "-Ywarn-unused-import"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oFD")
parallelExecution := false // <- until TMap thread-safety issues are resolved

scalacOptions in (Compile, doc) <++= (version,sourceDirectory in Compile,name).map((v,src,n) => Seq(
  "-doc-title", n,
  "-doc-version", v,
  "-doc-footer", projectName+" is developed by x.ai.",
  "-sourcepath", src.getPath, // needed for scaladoc to strip the location of the linked source path
  "-doc-source-url", ghUrl+"/blob/"+v+"/src/main€{FILE_PATH}.scala",
  "-implicits",
  "-diagrams", // requires graphviz
  "-groups"
))

publishTo := Some(
  if( version.value.trim.endsWith("SNAPSHOT") ){
    "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  } else {
    "releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  }
)

publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := { _ => false }
makePomConfiguration ~= { _.copy(configurations = Some(Seq(Compile, Runtime, Optional))) }
